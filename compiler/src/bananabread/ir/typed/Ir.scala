package bananabread
package ir
package typed

import error._
import parsing.ast
import parsing.ast.{Expr, Stmt}
import program.{ModuleSpace, ModDef, search}
import typechecker.{ty, infer, expect, signature, fresh, Scope, TypeScope}
import typechecker.ty.{Type, Void}
import typechecker.error.InferenceErr
import typechecker.unification.Substitution

import utils.{Ptr, PtrWith, squished, withonly}


type Lifted[T] = Either[LiftErr | InferenceErr, T]
type Scoped[T] = Lifted[(T, Scope)]


sealed trait Ir {
  def expr: Expr | Stmt
  def ty: Type
}

trait OfType(typ: Type) {
  def ty = typ
}

case class Num(expr: ast.Num, ty: Type) extends Ir
case class Str(expr: ast.Str) extends Ir, OfType(ty.Str) with PtrWith("str", () => expr.lexeme.hashCode)
case class Id(expr: ast.Id, ty: Type, source: ModDef) extends Ir
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


def lift(nodes: List[linked.Ir], space: ModuleSpace): Lifted[List[Ir]] =
  val sub = Substitution.empty
  val freshLocals = nodes.withonly[linked.Def]
    .map(_.name.lexeme)
    .map { name => (name, fresh()) }
    .toMap

  nodes.foldLeft[Scoped[List[Ir]]](Right((List.empty, freshLocals))) {
    case (Left(err), _) =>
      Left(err)
    case (Right((acc, scope)), node) =>
      lift(node, scope, sub, space).map { (ir, nextScope) =>
        (acc :+ ir, nextScope)
      }
  }.map(_._1)
def lift(node: linked.Ir, scope: Scope, sub: Substitution, space: ModuleSpace): Scoped[Ir] = node match
  case linked.Str(expr)    => Right((Str(expr), scope))
  case linked.Symbol(expr) => Right((Symbol(expr), scope))
  case linked.True(expr)   => Right((True(expr), scope))
  case linked.False(expr)  => Right((False(expr), scope))
  case linked.Opcode(expr) => Right((Opcode(expr), scope))
  case linked.Num(expr)    => Right((Num(expr, ty.I32), scope))
  case node: linked.Def    => liftDef(node, scope, sub, space)
  case node: linked.Id     => liftId(node, scope, sub, space)
  case node: linked.Lambda => liftLambda(node, scope, sub, space)
  case node: linked.App    => liftApp(node, scope, sub, space)
  case node: linked.Cond   => liftCond(node, scope, sub, space)
  case node: linked.Begin  => liftBegin(node, scope, sub, space)
  case node: linked.Let    => liftLet(node, scope, sub, space)

def liftId(node: linked.Id, scope: Scope, sub: Substitution, space: ModuleSpace): Scoped[Ir] =
  val expr = node.expr
  val label = expr.lexeme

  (scope.get(label), space.search(node.source, label)) match
    case (None, None) =>
      Left(UndeclaredIdentifierErr(node))
    case (_, Some(ir)) =>
      Right(Id(expr, ir.ty, node.source), scope)
    case (Some(ty), _) =>
      Right(Id(expr, ty, node.source), scope)

def liftDef(node: linked.Def, scope: Scope, sub: Substitution, space: ModuleSpace): Scoped[Def] =
  node.value match
    case value: linked.Lambda => liftDefLambda(node, value, scope, sub, space)
    case value                => liftDefValue(node, value, scope, sub, space)

def liftDefLambda(node: linked.Def, value: linked.Lambda, scope: Scope, sub: Substitution, space: ModuleSpace): Scoped[Def] =
  val name = node.name
  val expr = node.expr

  for
    sig <- signature(value)
    subScope = scope + (name.lexeme -> sig)
    lifted <- lift(value, subScope, sub, space).map { (lifted, _) =>
      (Def(name, lifted, expr, lifted.ty), scope + (name.lexeme -> lifted.ty))
    }
  yield
    lifted

def liftDefValue(node: linked.Def, value: linked.Ir, scope: Scope, sub: Substitution, space: ModuleSpace): Scoped[Def] =
  val name = node.name
  val expr = node.expr

  lift(value, scope, sub, space).map { (lifted, _) =>
    (Def(name, lifted, expr, lifted.ty), scope + (name.lexeme -> lifted.ty))
  }

def liftLambda(node: linked.Lambda, scope: Scope, sub: Substitution, space: ModuleSpace): Scoped[Lambda] =
  val tyVars = node.tyVars
  val params = node.params
  val body = node.body
  val expr = node.expr

  for
    inferredRes <- infer(node, scope, sub, space)
    (unknownTy, _) = inferredRes
    lamTy <- unknownTy.expect[ty.Lambda](node)
    tyScope = TypeScope.from(tyVars)
    inferredScope <- Scope.from(params, tyScope)
    liftedRes <- lift(body, scope ++ inferredScope, sub, space)
    (liftedBody, _) = liftedRes
    liftedParams = params.zip(lamTy.in).map { (param, ty) => Param(param.name, ty) }
  yield
    (Lambda(liftedParams, liftedBody, tyVars, expr, lamTy), scope)

def liftApp(node: linked.App, scope: Scope, sub: Substitution, space: ModuleSpace): Scoped[App] =
  val lambda = node.lambda
  val args = node.args
  val expr = node.expr

  for
    liftedLamRes <- lift(lambda, scope, sub, space)
    (liftedLam, _) = liftedLamRes
    liftedArgsRes <- args.map(lift(_, scope, sub, space)).squished
    liftedArgs = liftedArgsRes.map(_._1)
    inferredRes <- infer(node, scope, sub, space)
    (sig, _) = inferredRes
  yield
    (App(liftedLam, liftedArgs, expr, sig), scope)

def liftCond(node: linked.Cond, scope: Scope, sub: Substitution, space: ModuleSpace): Scoped[Cond] =
  val cond = node.cond
  val pass = node.pass
  val fail = node.fail
  val expr = node.expr

  for
    inferredRes <- infer(node, scope, sub, space)
    (ty, _) = inferredRes
    liftedResCond <- lift(cond, scope, sub, space)
    (liftedCond, _) = liftedResCond
    liftedResPass <- lift(pass, scope, sub, space)
    (liftedPass, _) = liftedResPass
    liftedResFail <- lift(fail, scope, sub, space)
    (liftedFail, _) = liftedResFail
  yield
    (Cond(liftedCond, liftedPass, liftedFail, expr, ty), scope)

def liftBegin(node: linked.Begin, scope: Scope, sub: Substitution, space: ModuleSpace): Scoped[Begin] =
  val ins = node.ins
  val expr = node.expr

  for
    liftedRes <- ins.map(lift(_, scope, sub, space)).squished
    liftedIns = liftedRes.map(_._1)
    ty = if liftedIns.isEmpty
         then Void
         else liftedIns.last.ty
  yield
    (Begin(liftedIns, expr, ty), scope)

def liftLet(node: linked.Let, scope: Scope, sub: Substitution, space: ModuleSpace): Scoped[Let] =
  val bindings = node.bindings
  val body = node.body
  val expr = node.expr

  for
    liftedBindingsRes <- bindings.foldLeft[Scoped[List[(linked.Binding, Ir)]]](Right((List.empty, scope))) {
      case (Left(err), _) => Left(err)
      case (Right((acc, scope)), binding) =>
        val recursiveScope =
          scope + (binding.label.lexeme -> fresh())
        for
          value <- lift(binding.value, recursiveScope, sub, space).map(_._1)
          ty = (binding.label.lexeme -> value.ty)
        yield
          (acc :+ (binding, value), scope + ty)
    }

    liftedParams = liftedBindingsRes._1
    lexicalScope = liftedBindingsRes._2

    liftedBody <- lift(body, lexicalScope, sub, space).map(_._1)
    liftedBindings = liftedParams.map { (binding, ir) =>
      Binding(binding.label, ir, binding.expr, ir.ty)
    }
  yield
    (Let(liftedBindings, liftedBody, expr, liftedBody.ty), scope)
