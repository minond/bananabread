package bananabread
package ir
package stitched

import error._
import parsing.ast
import parsing.ast.{Expr, Stmt}
import program.{ModuleSpace, search}
import typechecker.ty
import typechecker.ty.Type

import utils.{squished, Ptr, PtrWith}


case class Source(source: Option[ast.Import])
object Source:
  def local =
    Source(None)


sealed trait Ir {
  def expr: Expr | Stmt
  def ty: Type
}

trait OfType(typ: Type) {
  def ty = typ
}

case class Num(expr: ast.Num, ty: Type) extends Ir
case class Str(expr: ast.Str) extends Ir, OfType(ty.Str) with PtrWith("str", () => expr.lexeme.hashCode)
case class Id(expr: ast.Id, ty: Type, source: Source) extends Ir
case class Symbol(expr: ast.Symbol) extends Ir, OfType(ty.Symbol) with Ptr("symbol")
case class App(lambda: Ir, args: List[Ir], expr: Expr, ty: Type) extends Ir
case class Cond(cond: Ir, pass: Ir, fail: Ir, expr: Expr, ty: Type) extends Ir
case class Begin(ins: List[Ir], expr: Expr, ty: Type) extends Ir
case class Opcode(expr: ast.Opcode) extends Ir, OfType(ty.Void)
case class Def(name: ast.Id, value: Ir, expr: Stmt, ty: Type) extends Ir

case class Lambda(params: List[Param], body: Ir, tyVars: List[ast.TyId], expr: Expr, ty: Type) extends Ir with Ptr("lambda")
case class Param(name: ast.Id, ty: Type)

case class Let(bindings: List[Binding], body: Ir, expr: Expr, ty: Type) extends Ir
case class Binding(label: ast.Id, value: Ir, expr: ast.Binding, ty: Type)

sealed trait Bool extends Ir
case class True(expr: ast.True) extends Bool, OfType(ty.Bool)
case class False(expr: ast.False) extends Bool, OfType(ty.Bool)


type Lifted[T] = Either[LiftErr, T]
type Stitched = Lifted[Stitch]


case class Stitch(inline: List[Ir], top: List[Ir]):
  def stitch = top ++ inline
  def add(other: Stitch) =
    Stitch(this.inline ++ other.inline, this.top ++ other.top)
object Stitch:
  def empty = Stitch(List.empty[Ir], List.empty[Ir])

def inlined(node: Ir): Stitch =
  Stitch(List(node), List.empty[Ir])
def inlined(nodes: List[Ir]): Stitch =
  Stitch(nodes, List.empty[Ir])
def topped(node: Ir): Stitch =
  Stitch(List.empty[Ir], List(node))
def topped(nodes: List[Ir]): Stitch =
  Stitch(List.empty[Ir], nodes)

def stitchedInlined(node: Ir): Stitched =
  Right(inlined(node))
def stitchedInlined(nodes: List[Ir]): Stitched =
  Right(inlined(nodes))
def stitchedTop(node: Ir): Stitched =
  Right(topped(node))
def stitchedTop(nodes: List[Ir]): Stitched =
  Right(topped(nodes))


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
      lift(node, space).map { stitch => acc.add(stitch) }
  }.map(_.stitch)
def lift(node: typed.Ir, space: ModuleSpace): Stitched = node match
  case typed.Str(expr)     => stitchedInlined(Str(expr))
  case typed.Symbol(expr)  => stitchedInlined(Symbol(expr))
  case typed.True(expr)    => stitchedInlined(True(expr))
  case typed.False(expr)   => stitchedInlined(False(expr))
  case typed.Opcode(expr)  => stitchedInlined(Opcode(expr))
  case typed.Num(expr, ty) => stitchedInlined(Num(expr, ty))
  case node: typed.Def     => liftDef(node, space)
  case node: typed.Id      => liftId(node, space)
  case node: typed.Lambda  => liftLambda(node, space)
  case node: typed.App     => liftApp(node, space)
  case node: typed.Cond    => liftCond(node, space)
  case node: typed.Begin   => liftBegin(node, space)
  case node: typed.Let     => liftLet(node, space)

def liftId(node: typed.Id, space: ModuleSpace): Stitched =
  val expr = node.expr
  val source = node.source
  val label = expr.lexeme
  val ty = node.ty

  val id = Id(expr, ty, Source(source))

  (source, space.search(source, label)) match
    case (None, _) => stitchedInlined(id)
    case (Some(_), Some(definition)) =>
      for
        liftedDef <- lift(definition, space)
      yield
        Stitch(List(id), liftedDef.stitch)
    case (Some(home), None) =>
      Left(MissingSourceErr(node))

def liftDef(node: typed.Def, space: ModuleSpace): Stitched =
  val name = node.name
  val value = node.value
  val expr = node.expr
  val ty = node.ty

  for
    liftedValueRes <- lift(value, space)
    liftedValue <- liftedValueRes.inline.single(value)
  yield
    Stitch(List(Def(name, liftedValue, expr, ty)), liftedValueRes.top)

def liftLambda(node: typed.Lambda, space: ModuleSpace): Stitched =
  val tyVars = node.tyVars
  val params = node.params
  val body = node.body
  val expr = node.expr
  val ty = node.ty

  for
    liftedBodyRes <- lift(body, space)
    liftedBody <- liftedBodyRes.inline.single(body)
    liftedParams = params.map { p => Param(p.name, p.ty) }
  yield
    Stitch(List(Lambda(liftedParams, liftedBody, tyVars, expr, ty)), liftedBodyRes.top)

def liftApp(node: typed.App, space: ModuleSpace): Stitched =
  val lambda = node.lambda
  val args = node.args
  val expr = node.expr
  val ty = node.ty

  for
    liftedLamRes  <- lift(lambda, space)
    liftedLam     <- liftedLamRes.inline.single(lambda)
    liftedArgsRes <- args.map(lift(_, space)).squished

    liftedArgs = liftedArgsRes.map(_.inline).flatten
    top        = liftedLamRes.top ++ liftedArgsRes.map(_.top).flatten
  yield
    Stitch(List(App(liftedLam, liftedArgs, expr, ty)), top)

def liftCond(node: typed.Cond, space: ModuleSpace): Stitched =
  val cond = node.cond
  val pass = node.pass
  val fail = node.fail
  val expr = node.expr
  val ty = node.ty

  for
    liftedCondRes <- lift(cond, space)
    liftedPassRes <- lift(pass, space)
    liftedFailRes <- lift(fail, space)

    inlineCond <- liftedCondRes.inline.single(cond)
    inlinePass <- liftedPassRes.inline.single(pass)
    inlineFail <- liftedFailRes.inline.single(fail)

    top = liftedCondRes.top ++ liftedPassRes.top ++ liftedFailRes.top
  yield
    Stitch(List(Cond(inlineCond, inlinePass, inlineFail, expr, ty)), top)

def liftBegin(node: typed.Begin, space: ModuleSpace): Stitched =
  val ins = node.ins
  val expr = node.expr
  val ty = node.ty

  for
    liftedRes <- ins.map(lift(_, space)).squished
    restitched = liftedRes.flat
  yield
    Stitch(List(Begin(restitched.inline, expr, ty)), restitched.top)

def liftLet(node: typed.Let, space: ModuleSpace): Stitched =
  val bindings = node.bindings
  val body = node.body
  val expr = node.expr
  val ty = node.ty

  for
    liftedBodyRes <- lift(body, space)
    liftedBody <- liftedBodyRes.inline.single(body)

    liftedBindingsRes <- bindings.foldLeft[Lifted[List[Stitch]]](Right(List.empty)) {
      case (Left(err), _) => Left(err)
      case (Right(acc), binding) =>
        for
          liftedBindingRes <- lift(binding.value, space)
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
    Stitch(List(Let(liftedBindings, liftedBody, expr, ty)), top)
