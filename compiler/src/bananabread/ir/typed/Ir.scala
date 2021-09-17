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
case class Symbol(expr: ast.Id) extends Ir, OfType(ty.Symbol)
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
  case typeless.Def(name, value: typeless.Lambda, expr) =>
    for
      sig <- signature(value)
      subScope = scope + (name.lexeme -> sig)
      lifted <- lift(value, subScope).map { (lifted, _) =>
        (Def(name, lifted, expr, lifted.ty), scope + (name.lexeme -> lifted.ty))
      }
    yield
      lifted

  case typeless.Def(name, value, expr) =>
    lift(value, scope).map { (lifted, _) =>
      (Def(name, lifted, expr, lifted.ty), scope + (name.lexeme -> lifted.ty))
    }

  case node @ typeless.Lambda(params, body, tyVars, expr) =>
    for
      inferredRes <- infer(node, scope)
      (ty, _) = inferredRes
      tyScope = TypeScope.from(tyVars)
      inferredScope <- Scope.from(params, tyScope)
      liftedRes <- lift(body, scope ++ inferredScope)
      (liftedBody, _) = liftedRes
    yield
      (Lambda(params, liftedBody, tyVars, expr, ty), scope)

  case typeless.Opcode(expr) =>
    Right((Opcode(expr), scope))

  case node @ typeless.Cond(cond, pass, fail, expr) =>
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

  case node @ typeless.Id(expr) =>
    scope.get(expr.lexeme) match
      case None => Left(UndeclaredIdentifierErr(node))
      case Some(ty) => Right(Id(expr, ty), scope)

  case node @ typeless.App(lam, args, expr) =>
    for
      liftedLamRes <- lift(lam, scope)
      (liftedLam, _) = liftedLamRes
      liftedArgsRes <- args.map(lift(_, scope)).squished
      liftedArgs = liftedArgsRes.map(_._1)
      inferredRes <- infer(node, scope)
      (sig, _) = inferredRes
    yield
      (App(liftedLam, liftedArgs, expr, sig), scope)

  case typeless.Num(expr) =>
    Right((Num(expr, ty.I32), scope))

  case typeless.Begin(ins, expr) =>
    for
      liftedRes <- ins.map(lift(_, scope)).squished
      liftedIns = liftedRes.map(_._1)
      ty = if liftedIns.isEmpty
           then Void
           else liftedIns.last.ty
    yield
      (Begin(liftedIns, expr, ty), scope)
