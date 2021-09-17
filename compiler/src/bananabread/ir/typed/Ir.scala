package bananabread
package ir
package typed

import error._
import parsing.ast
import parsing.ast.{Expr, Stmt}
import typechecker.{ty, infer, Scope, TypeScope}
import typechecker.ty.Type
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
