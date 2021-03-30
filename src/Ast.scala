package bisquit
package ast


// Base AST

sealed class Token(loc: Location) extends Located, At(loc)
sealed trait Expr extends Located
sealed trait Literal extends Expr


// Tokens

case class Eof(loc: Location) extends Token(loc)
case class Comma(loc: Location) extends Token(loc)
case class Dot(loc: Location) extends Token(loc)
case class OpenParen(loc: Location) extends Token(loc)
case class CloseParen(loc: Location) extends Token(loc)
case class OpenCurlyParen(loc: Location) extends Token(loc)
case class CloseCurlyParen(loc: Location) extends Token(loc)
case class OpenSquareBraket(loc: Location) extends Token(loc)
case class CloseSquareBraket(loc: Location) extends Token(loc)


// Token expressions

case class Num(lexeme: String, loc: Location) extends Token(loc) with Expr, Literal
case class Str(lexeme: String, loc: Location) extends Token(loc) with Expr, Literal
case class Id(lexeme: String, loc: Location) extends Token(loc) with Expr, Literal


// Expressions

case class Binop(lhs: Expr, op: Id, rhs: Expr) extends Expr with At(op.location)
case class Uniop(op: Id, rhs: Expr) extends Expr with At(op.location)
case class App(lambda: Expr, args: List[Expr]) extends Expr with At(lambda.location)


// Errors

sealed trait SyntaxErr(loc: Location) extends Located { def location = loc }
case class BadNumErr(lexeme: String, loc: Location) extends SyntaxErr(loc)
case class UnexpectedEofErr(prev: Located) extends SyntaxErr(prev.location)


// Token/expression location information

case class Location(source: String, offset: Int)
trait Located { def location: Location }
trait At(loc: Location) { def location = loc }
