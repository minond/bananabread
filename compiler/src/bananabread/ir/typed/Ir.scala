package bananabread
package ir
package typed

import error._
import parsing.ast
import parsing.ast.{Expr, Stmt}
import typechecker.{ty, infer, signature, Scope, TypeScope}
import typechecker.ty.{Type, Void}
import typechecker.error.InferenceErr

import utils.squished


type Lifted[T] = Either[LiftErr | InferenceErr, T]


sealed trait Ir {
  def expr: Expr | Stmt
  def ty: Type
}

trait OfType(typ: Type) {
  def ty = typ
}

case class Num(expr: ast.Num, ty: Type) extends Ir
case class Str(expr: ast.Str) extends Ir, OfType(ty.Str)
case class Id(expr: Expr, ty: Type) extends Ir
case class Symbol(expr: ast.Symbol) extends Ir, OfType(ty.Symbol)
case class App(lambda: Ir, args: List[Ir], expr: Expr, ty: Type) extends Ir
case class Lambda(params: List[ast.Param], body: Ir, tyVars: List[ast.TyId], expr: Expr, ty: Type) extends Ir
case class Cond(cond: Ir, pass: Ir, fail: Ir, expr: Expr, ty: Type) extends Ir
case class Begin(ins: List[Ir], expr: Expr, ty: Type) extends Ir
case class Opcode(expr: ast.Opcode) extends Ir, OfType(ty.Void)
case class Def(name: ast.Id, value: Ir, expr: Stmt, ty: Type) extends Ir

case class Let(bindings: List[Binding], body: Ir, expr: Expr, ty: Type) extends Ir
case class Binding(label: ast.Id, value: Ir, expr: ast.Binding, ty: Type)

case class True(expr: ast.True) extends Ir, OfType(ty.Bool)
case class False(expr: ast.False) extends Ir, OfType(ty.Bool)


def lift(nodes: List[typeless.Ir]): Lifted[List[Ir]] =
  nodes.foldLeft[Lifted[(List[Ir], Scope)]](Right((List.empty, Scope.empty))) {
    case (Left(err), _) =>
      Left(err)
    case (Right((acc, scope)), node) =>
      lift(node, scope).map { (ir, nextScope) =>
        (acc :+ ir, nextScope)
      }
  }.map(_._1)
def lift(node: typeless.Ir, scope: Scope): Lifted[(Ir, Scope)] = node match
  case typeless.Str(expr)    => Right((Str(expr), scope))
  case typeless.Symbol(expr) => Right((Symbol(expr), scope))
  case typeless.True(expr)   => Right((True(expr), scope))
  case typeless.False(expr)  => Right((False(expr), scope))
  case typeless.Opcode(expr) => Right((Opcode(expr), scope))
  case typeless.Num(expr)    => Right((Num(expr, ty.I32), scope))
  case node: typeless.Def    => liftDef(node, scope)
  case node: typeless.Id     => liftId(node, scope)
  case node: typeless.Lambda => liftLambda(node, scope)
  case node: typeless.App    => liftApp(node, scope)
  case node: typeless.Cond   => liftCond(node, scope)
  case node: typeless.Begin  => liftBegin(node, scope)
  case node: typeless.Let    => liftLet(node, scope)

def liftId(node: typeless.Id, scope: Scope): Lifted[(Ir, Scope)] =
  val expr = node.expr

  scope.get(expr.lexeme) match
    case None => Left(UndeclaredIdentifierErr(node))
    case Some(ty) => Right(Id(expr, ty), scope)

def liftDef(node: typeless.Def, scope: Scope): Lifted[(Def, Scope)] =
  node.value match
    case value: typeless.Lambda => liftDefLambda(node, value, scope)
    case value                  => liftDefValue(node, value, scope)

def liftDefLambda(node: typeless.Def, value: typeless.Lambda, scope: Scope): Lifted[(Def, Scope)] =
  val name = node.name
  val expr = node.expr

  for
    sig <- signature(value)
    subScope = scope + (name.lexeme -> sig)
    lifted <- lift(value, subScope).map { (lifted, _) =>
      (Def(name, lifted, expr, lifted.ty), scope + (name.lexeme -> lifted.ty))
    }
  yield
    lifted

def liftDefValue(node: typeless.Def, value: typeless.Ir, scope: Scope): Lifted[(Def, Scope)] =
  val name = node.name
  val expr = node.expr

  lift(value, scope).map { (lifted, _) =>
    (Def(name, lifted, expr, lifted.ty), scope + (name.lexeme -> lifted.ty))
  }

def liftLambda(node: typeless.Lambda, scope: Scope): Lifted[(Lambda, Scope)] =
  val tyVars = node.tyVars
  val params = node.params
  val body = node.body
  val expr = node.expr

  for
    inferredRes <- infer(node, scope)
    (ty, _) = inferredRes
    tyScope = TypeScope.from(tyVars)
    inferredScope <- Scope.from(params, tyScope)
    liftedRes <- lift(body, scope ++ inferredScope)
    (liftedBody, _) = liftedRes
  yield
    (Lambda(params, liftedBody, tyVars, expr, ty), scope)

def liftApp(node: typeless.App, scope: Scope): Lifted[(App, Scope)] =
  val lambda = node.lambda
  val args = node.args
  val expr = node.expr

  for
    liftedLamRes <- lift(lambda, scope)
    (liftedLam, _) = liftedLamRes
    liftedArgsRes <- args.map(lift(_, scope)).squished
    liftedArgs = liftedArgsRes.map(_._1)
    inferredRes <- infer(node, scope)
    (sig, _) = inferredRes
  yield
    (App(liftedLam, liftedArgs, expr, sig), scope)

def liftCond(node: typeless.Cond, scope: Scope): Lifted[(Cond, Scope)] =
  val cond = node.cond
  val pass = node.pass
  val fail = node.fail
  val expr = node.expr

  for
    inferredRes <- infer(node, scope)
    (ty, _) = inferredRes
    liftedResCond <- lift(cond, scope)
    (liftedCond, _) = liftedResCond
    liftedResPass <- lift(pass, scope)
    (liftedPass, _) = liftedResPass
    liftedResFail <- lift(fail, scope)
    (liftedFail, _) = liftedResFail
  yield
    (Cond(liftedCond, liftedPass, liftedFail, expr, ty), scope)

def liftBegin(node: typeless.Begin, scope: Scope): Lifted[(Begin, Scope)] =
  val ins = node.ins
  val expr = node.expr

  for
    liftedRes <- ins.map(lift(_, scope)).squished
    liftedIns = liftedRes.map(_._1)
    ty = if liftedIns.isEmpty
         then Void
         else liftedIns.last.ty
  yield
    (Begin(liftedIns, expr, ty), scope)

def liftLet(node: typeless.Let, scope: Scope): Lifted[(Let, Scope)] =
  ???
