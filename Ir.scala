package sourdough
package ir

import ast.Expr
import ty.Type


sealed trait Ir(expr: Expr, ty: Type)
case class Num(expr: Expr, ty: Type) extends Ir(expr, ty)
case class Str(expr: Expr, ty: Type) extends Ir(expr, ty)
case class Id(expr: Expr, ty: Type) extends Ir(expr, ty)
case class App(lambda: Ir, args: List[Ir], expr: Expr, ty: Type) extends Ir(expr, ty)
case class Lambda(params: List[Ir], body: Ir, expr: Expr, ty: Type) extends Ir(expr, ty)

/** Simplifies the source tree by removing all operators, leaving only
  * literals, functions, and function application.
  */
enum Typeless:
  case Num(expr: Expr)
  case Str(expr: Expr)
  case Id(expr: Expr)
  case App(lambda: Typeless, args: List[Typeless], expr: Expr)
  case Lambda(params: List[Typeless], body: Typeless, expr: Expr)

  override def toString: String = this match
    case Typeless.Num(expr) => s"(num ${expr.toString})"
    case Typeless.Str(expr) => s"(str ${expr.toString})"
    case Typeless.Id(expr) => s"(id ${expr.toString})"
    case Typeless.App(lambda, args, _) => s"(app lambda: ${lambda} args: (${args.mkString(" ")}))"
    case Typeless.Lambda(params, body, _) => s"(lambda params: (${params.mkString(" ")}) body: $body)"

object Typeless:
  def lift(exprs: List[Expr]): List[Typeless] =
    exprs.map(lift)

  def lift(expr: Expr): Typeless = expr match
    case _: ast.Num => Typeless.Num(expr)
    case _: ast.Str => Typeless.Str(expr)
    case _: ast.Id => Typeless.Id(expr)
    case ast.App(lambda, args) => Typeless.App(lift(lambda), args.map(lift), expr)
    case ast.Lambda(params, body) => Typeless.Lambda(params.map(lift), lift(body), expr)
    case ast.Uniop(op, operand) => Typeless.App(lift(op), List(operand).map(lift), expr)
    case ast.Binop(op, lhs, rhs) => Typeless.App(lift(op), List(lhs, rhs).map(lift), expr)
