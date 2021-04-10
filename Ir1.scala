package sourdough
package ir1


/** Simplifies the source tree by removing all operators, leaving only
  * literals, functions, and function application.
  */
sealed trait Ir1:
  self =>
    override def toString: String = self match
      case Num(expr) => s"(num ${expr.toString})"
      case Str(expr) => s"(str ${expr.toString})"
      case Id(expr) => s"(id ${expr.toString})"
      case App(lambda, args, _) => s"(app lambda: ${lambda} args: (${args.mkString(" ")}))"
      case Lambda(params, body, _) => s"(lambda params: (${params.mkString(" ")}) body: $body)"

case class Num(expr: ast.Expr) extends Ir1
case class Str(expr: ast.Expr) extends Ir1
case class Id(expr: ast.Expr) extends Ir1
case class App(lambda: Ir1, args: List[Ir1], expr: ast.Expr) extends Ir1
case class Lambda(params: List[Ir1], body: Ir1, expr: ast.Expr) extends Ir1

def lift(exprs: List[ast.Expr]): List[Ir1] =
  exprs.map(lift)

def lift(expr: ast.Expr): Ir1 = expr match
  case _: ast.Num => Num(expr)
  case _: ast.Str => Str(expr)
  case _: ast.Id => Id(expr)
  case ast.App(lambda, args) => App(lift(lambda), args.map(lift), expr)
  case ast.Lambda(params, body) => Lambda(params.map(lift), lift(body), expr)
  case ast.Uniop(op, operand) => App(lift(op), List(operand).map(lift), expr)
  case ast.Binop(op, lhs, rhs) => App(lift(op), List(lhs, rhs).map(lift), expr)
