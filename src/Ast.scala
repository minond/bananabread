package bisquit
package ast

case class Location(source: String, offset: Int)
trait Located { def location: Location }
trait At(loc: Location) { def location = loc }

sealed class Token(loc: Location) extends Located, At(loc)
sealed trait Expr extends Located

case class Eof(loc: Location) extends Token(loc)

case class Num(lexeme: String, loc: Location) extends Token(loc) with Expr
case class Str(lexeme: String, loc: Location) extends Token(loc) with Expr
case class Id(lexeme: String, loc: Location) extends Token(loc) with Expr

case class Binop(lhs: Token, op: Token, rhs: Token) extends Expr with At(op.location)
case class Uniop(op: Token, rhs: Token) extends Expr with At(op.location)
case class App(lambda: Expr, args: List[Expr]) extends Expr with At(lambda.location)

sealed trait SyntaxErr(loc: Location) extends Located { def location = loc }
case class BadNumErr(lexeme: String, loc: Location) extends SyntaxErr(loc)
