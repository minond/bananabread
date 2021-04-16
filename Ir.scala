package sourdough
package ir

import ast.Expr
import ty.Type
import utils.Print


sealed trait Ir(expr: Expr, ty: Type)
case class Num(num: ast.Num, ty: Type) extends Ir(num, ty)
case class Str(str: ast.Str) extends Ir(str, ty.Str)
case class Id(expr: Expr, ty: Type) extends Ir(expr, ty)
case class App(lambda: Ir, args: List[Ir], expr: Expr, ty: Type) extends Ir(expr, ty)
case class Lambda(params: List[Ir], body: Ir, expr: Expr, ty: Type) extends Ir(expr, ty)
case class Cond(cond: Ir, pass: Ir, fail: Ir, expr: Expr, ty: Type) extends Ir(expr, ty)
case class Let(bindings: List[Binding], body: Ir, expr: Expr, ty: Type) extends Ir(expr, ty)

case class Binding(label: ast.Id, value: Ir, expr: ast.Binding, ty: Type)

/** Simplifies the source tree by removing all operators, leaving only
  * literals, functions, and function application.
  */
object Typeless:
  sealed trait Ir
  case class Num(num: ast.Num) extends Ir with Print(s"(num ${num.lexeme})")
  case class Str(str: ast.Str) extends Ir with Print(s"(str ${str.lexeme})")
  case class Id(id: ast.Id) extends Ir with Print(s"(id ${id.lexeme})")
  case class App(lambda: Ir, args: List[Ir], expr: Expr) extends Ir with Print(s"(app lambda: ${lambda} args: (${args.mkString(" ")}))")
  case class Lambda(params: List[Ir], body: Ir, expr: Expr) extends Ir with Print(s"(lambda params: (${params.mkString(" ")}) body: $body)")
  case class Cond(cond: Ir, pass: Ir, fail: Ir, expr: Expr) extends Ir with Print(s"(if cond: $cond then: $pass else: $fail)")
  case class Let(bindings: List[Binding], body: Ir, expr: Expr) extends Ir with Print(s"(let bindings: (${bindings.mkString(" ")}) body: $body)")

  case class Binding(label: ast.Id, value: Ir, expr: ast.Binding) extends Print(s"binding: $label value: $value")

  def lift(exprs: List[Expr]): List[Ir] =
    exprs.map(lift)

  def lift(expr: Expr): Ir = expr match
    case num: ast.Num => Num(num)
    case str: ast.Str => Str(str)
    case id: ast.Id => Id(id)
    case ast.App(lambda, args) => App(lift(lambda), args.map(lift), expr)
    case ast.Lambda(params, body) => Lambda(params.map(lift), lift(body), expr)
    case ast.Uniop(op, operand) => App(lift(op), List(operand).map(lift), expr)
    case ast.Binop(op, lhs, rhs) => App(lift(op), List(lhs, rhs).map(lift), expr)
    case ast.Cond(_, cond, pass, fail) => Cond(lift(cond), lift(pass), lift(fail), expr)
    case ast.Let(_, bindings, body) => Let(bindings.map(lift), lift(body), expr)

  def lift(binding: ast.Binding): Binding =
    Binding(binding.label, lift(binding.value), binding)
