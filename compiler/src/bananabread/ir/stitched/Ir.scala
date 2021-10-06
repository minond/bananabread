package bananabread
package ir
package stitched

import error._
import parsing.ast
import parsing.ast.{Expr, Stmt}
import program.{ModuleSpace, search, locate}
import typechecker.ty
import typechecker.ty.Type

import utils.{squished, Ptr, PtrWith}


case class Module(name: String)
object Module:
  def main = Module("main")
  def from(maybeSource: Option[ast.Ref]) = maybeSource match
    case None => main
    case Some(ref) => Module(ref.id.lexeme)
  def from(pmod: program.Module) =
    Module(pmod.defn.name)


sealed trait Ir {
  def expr: Expr | Stmt
  def ty: Type
  def source: Module
}

trait OfType(typ: Type) {
  def ty = typ
}

case class Num(expr: ast.Num, ty: Type, source: Module) extends Ir
case class Str(expr: ast.Str, source: Module) extends Ir, OfType(ty.Str) with PtrWith("str", () => expr.lexeme.hashCode)
case class Id(expr: ast.Id, ty: Type, source: Module) extends Ir
case class Symbol(expr: ast.Symbol, source: Module) extends Ir, OfType(ty.Symbol) with Ptr("symbol")
case class App(lambda: Ir, args: List[Ir], expr: Expr, ty: Type, source: Module) extends Ir
case class Cond(cond: Ir, pass: Ir, fail: Ir, expr: Expr, ty: Type, source: Module) extends Ir
case class Begin(ins: List[Ir], expr: Expr, ty: Type, source: Module) extends Ir
case class Opcode(expr: ast.Opcode, source: Module) extends Ir, OfType(ty.Void)
case class Def(name: ast.Id, value: Ir, expr: Stmt, ty: Type, source: Module) extends Ir

case class Lambda(params: List[Param], body: Ir, tyVars: List[ast.TyId], expr: Expr, ty: Type, source: Module) extends Ir with Ptr("lambda")
case class Param(name: ast.Id, ty: Type)

case class Let(bindings: List[Binding], body: Ir, expr: Expr, ty: Type, source: Module) extends Ir
case class Binding(label: ast.Id, value: Ir, expr: ast.Binding, ty: Type)

sealed trait Bool extends Ir
case class True(expr: ast.True, source: Module) extends Bool, OfType(ty.Bool)
case class False(expr: ast.False, source: Module) extends Bool, OfType(ty.Bool)


type Lifted[T] = Either[LiftErr, T]
type Stitched = Lifted[Stitch]


case class Stitch(inline: List[Ir], top: List[Ir]):
  def stitch = top ++ inline
  def above(node: Ir) = Stitch(this.inline, top :+ node)
  def above(nodes: List[Ir]) = Stitch(this.inline, top ++ nodes)
  def add(other: Stitch) =
    Stitch(this.inline ++ other.inline, this.top ++ other.top)
object Stitch:
  def empty =
    Stitch(List.empty[Ir], List.empty[Ir])
  def inlined(node: Ir) = Stitch(List(node), List.empty)
  def inlined(nodes: List[Ir]) = Stitch(nodes, List.empty)


extension (stitches: List[Stitch])
  def flat: Stitch =
    stitches.foldLeft(Stitch.empty) {
      (acc, stitch) =>
        acc.add(stitch)
    }


extension (nodes: List[Ir])
  def single(original: typed.Ir): Either[ExpectedSingleLiftedMore, Ir] =
    if nodes.size == 1
    then Right(nodes.head)
    else Left(ExpectedSingleLiftedMore(original))

def lift(nodes: List[typed.Ir], space: ModuleSpace): Lifted[List[Ir]] =
  nodes.foldLeft[Lifted[Stitch]](Right(Stitch.empty)) {
    case (Left(err), _) =>
      Left(err)
    case (Right(acc), node) =>
      lift(node, Module.main, space).map { stitch => acc.add(stitch) }
  }.map(_.stitch)
def lift(node: typed.Ir, source: Module, space: ModuleSpace): Stitched = node match
  case typed.Str(expr)     => Right(Stitch.inlined(Str(expr, source)))
  case typed.Symbol(expr)  => Right(Stitch.inlined(Symbol(expr, source)))
  case typed.True(expr)    => Right(Stitch.inlined(True(expr, source)))
  case typed.False(expr)   => Right(Stitch.inlined(False(expr, source)))
  case typed.Opcode(expr)  => Right(Stitch.inlined(Opcode(expr, source)))
  case typed.Num(expr, ty) => Right(Stitch.inlined(Num(expr, ty, source)))
  case node: typed.Def     => liftDef(node, source, space)
  case node: typed.Id      => liftId(node, source, space)
  case node: typed.Lambda  => liftLambda(node, source, space)
  case node: typed.App     => liftApp(node, source, space)
  case node: typed.Cond    => liftCond(node, source, space)
  case node: typed.Begin   => liftBegin(node, source, space)
  case node: typed.Let     => liftLet(node, source, space)

/** This was returning a `MissingSourceErr` error when the node had a souce but
  * it was not found in the module space. This upstream irs are not setting
  * sources more often, this got changed to assume that a missing module in the
  * space is due to the node being defined in the main entry module. One way to
  * update this code to no longer assume that would be to somehow add the main
  * module to the module space.
  */
def liftId(node: typed.Id, source: Module, space: ModuleSpace): Stitched =
  val expr = node.expr
  val label = expr.lexeme
  val ty = node.ty
  val id = Id(expr, ty, Module.from(node.source))

  space.locate(node.source, label) match
    case (Some(mod), Some(definition)) =>
      for
        liftedDef <- lift(definition, Module.from(mod), space)
      yield
        Stitch.inlined(id).above(liftedDef.stitch)
    case _ => Right(Stitch.inlined(id))

def liftDef(node: typed.Def, source: Module, space: ModuleSpace): Stitched =
  val name = node.name
  val value = node.value
  val expr = node.expr
  val ty = node.ty

  for
    liftedValueRes <- lift(value, source, space)
    liftedValue <- liftedValueRes.inline.single(value)
  yield
    Stitch.inlined(Def(name, liftedValue, expr, ty, source)).above(liftedValueRes.top)

def liftLambda(node: typed.Lambda, source: Module, space: ModuleSpace): Stitched =
  val tyVars = node.tyVars
  val params = node.params
  val body = node.body
  val expr = node.expr
  val ty = node.ty

  for
    liftedBodyRes <- lift(body, source, space)
    liftedBody <- liftedBodyRes.inline.single(body)
    liftedParams = params.map { p => Param(p.name, p.ty) }
  yield
    Stitch.inlined(Lambda(liftedParams, liftedBody, tyVars, expr, ty, source)).above(liftedBodyRes.top)

def liftApp(node: typed.App, source: Module, space: ModuleSpace): Stitched =
  val lambda = node.lambda
  val args = node.args
  val expr = node.expr
  val ty = node.ty

  for
    liftedLamRes  <- lift(lambda, source, space)
    liftedLam     <- liftedLamRes.inline.single(lambda)
    liftedArgsRes <- args.map(lift(_, source, space)).squished

    liftedArgs = liftedArgsRes.map(_.inline).flatten
    top        = liftedLamRes.top ++ liftedArgsRes.map(_.top).flatten
  yield
    Stitch.inlined(App(liftedLam, liftedArgs, expr, ty, source)).above(top)

def liftCond(node: typed.Cond, source: Module, space: ModuleSpace): Stitched =
  val cond = node.cond
  val pass = node.pass
  val fail = node.fail
  val expr = node.expr
  val ty = node.ty

  for
    liftedCondRes <- lift(cond, source, space)
    liftedPassRes <- lift(pass, source, space)
    liftedFailRes <- lift(fail, source, space)

    inlineCond <- liftedCondRes.inline.single(cond)
    inlinePass <- liftedPassRes.inline.single(pass)
    inlineFail <- liftedFailRes.inline.single(fail)

    top = liftedCondRes.top ++ liftedPassRes.top ++ liftedFailRes.top
  yield
    Stitch.inlined(Cond(inlineCond, inlinePass, inlineFail, expr, ty, source)).above(top)

def liftBegin(node: typed.Begin, source: Module, space: ModuleSpace): Stitched =
  val ins = node.ins
  val expr = node.expr
  val ty = node.ty

  for
    liftedRes <- ins.map(lift(_, source, space)).squished
    restitched = liftedRes.flat
  yield
    Stitch.inlined(Begin(restitched.inline, expr, ty, source)).above(restitched.top)

def liftLet(node: typed.Let, source: Module, space: ModuleSpace): Stitched =
  val bindings = node.bindings
  val body = node.body
  val expr = node.expr
  val ty = node.ty

  for
    liftedBodyRes <- lift(body, source, space)
    liftedBody <- liftedBodyRes.inline.single(body)

    liftedBindingsRes <- bindings.foldLeft[Lifted[List[Stitch]]](Right(List.empty)) {
      case (Left(err), _) => Left(err)
      case (Right(acc), binding) =>
        for
          liftedBindingRes <- lift(binding.value, source, space)
          _ <- liftedBindingRes.inline.single(binding.value)
        yield
          acc :+ liftedBindingRes
    }

    liftedBindings = liftedBindingsRes.map(_.inline.head).zip(bindings).map {
      (ir, binding) =>
        Binding(binding.label, ir, binding.expr, binding.ty)
    }

    top = liftedBodyRes.top ++ liftedBindingsRes.map(_.top).flatten
  yield
    Stitch.inlined(Let(liftedBindings, liftedBody, expr, ty, source)).above(top)
