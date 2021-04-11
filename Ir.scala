package sourdough
package ir

import ast.Expr
import ty.Type


sealed trait Ir(expr: Expr, ty: Type)
case class Num(num: ast.Num, ty: Type) extends Ir(num, ty)
case class Str(str: ast.Str) extends Ir(str, ty.Str)
case class Id(expr: Expr, ty: Type) extends Ir(expr, ty)
case class App(lambda: Ir, args: List[Ir], expr: Expr, ty: Type) extends Ir(expr, ty)
case class Lambda(params: List[Ir], body: Ir, expr: Expr, ty: Type) extends Ir(expr, ty)

/** Simplifies the source tree by removing all operators, leaving only
  * literals, functions, and function application.
  */
enum Typeless:
  case Num(num: ast.Num)
  case Str(str: ast.Str)
  case Id(id: ast.Id)
  case App(lambda: Typeless, args: List[Typeless], expr: Expr)
  case Lambda(params: List[Typeless], body: Typeless, expr: Expr)

  override def toString: String = this match
    case Typeless.Num(num) => s"(num ${num.lexeme})"
    case Typeless.Str(str) => s"(str ${str.lexeme})"
    case Typeless.Id(id) => s"(id ${id.lexeme})"
    case Typeless.App(lambda, args, _) => s"(app lambda: ${lambda} args: (${args.mkString(" ")}))"
    case Typeless.Lambda(params, body, _) => s"(lambda params: (${params.mkString(" ")}) body: $body)"

object Typeless:
  def lift(exprs: List[Expr]): List[Typeless] =
    exprs.map(lift)

  def lift(expr: Expr): Typeless = expr match
    case num: ast.Num => Typeless.Num(num)
    case str: ast.Str => Typeless.Str(str)
    case id: ast.Id => Typeless.Id(id)
    case ast.App(lambda, args) => Typeless.App(lift(lambda), args.map(lift), expr)
    case ast.Lambda(params, body) => Typeless.Lambda(params.map(lift), lift(body), expr)
    case ast.Uniop(op, operand) => Typeless.App(lift(op), List(operand).map(lift), expr)
    case ast.Binop(op, lhs, rhs) => Typeless.App(lift(op), List(lhs, rhs).map(lift), expr)
