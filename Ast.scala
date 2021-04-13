package sourdough
package ast


// Base AST

sealed class Token(loc: Location) extends Located, At(loc)
sealed trait Expr extends Located
sealed trait Literal extends Expr


// Tokens

case class Eof(loc: Location) extends Token(loc) with Print()
case class Comma(loc: Location) extends Token(loc) with Print()
case class Dot(loc: Location) extends Token(loc) with Print()
case class OpenParen(loc: Location) extends Token(loc) with Print()
case class CloseParen(loc: Location) extends Token(loc) with Print()
case class OpenCurlyParen(loc: Location) extends Token(loc) with Print()
case class CloseCurlyParen(loc: Location) extends Token(loc) with Print()
case class OpenSquareBraket(loc: Location) extends Token(loc) with Print()
case class CloseSquareBraket(loc: Location) extends Token(loc) with Print()


// Token expressions

case class Num(lexeme: String, loc: Location) extends Token(loc) with Expr, Literal, Print(lexeme)
case class Str(lexeme: String, loc: Location) extends Token(loc) with Expr, Literal, Print(lexeme)
case class Id(lexeme: String, loc: Location) extends Token(loc) with Expr, Literal, Print(lexeme)


// Expressions

case class Binop(op: Id, lhs: Expr, rhs: Expr) extends Expr with At(op.location), Print(s"($op $lhs $rhs)")
case class Uniop(op: Id, operand: Expr) extends Expr with At(op.location), Print(s"($op $operand)")
case class App(lambda: Expr, args: List[Expr]) extends Expr with At(lambda.location), Print(s"(${(lambda +: args).mkString(" ")})")
case class Lambda(params: List[Expr], body: Expr) extends Expr with At(body.location), Print(s"{${params.mkString(", ")} = $body}")
case class Cond(start: Token, cond: Expr, pass: Expr, fail: Expr) extends Expr with At(start.location), Print(s"if $cond then $pass else $fail")
case class Let(start: Token, bindings: List[Binding], body: Expr) extends Expr with At(start.location), Print(s"let ${bindings.mkString(" ")} in $body")


// Aux expressions
case class Binding(label: Id, value: Expr) extends At(label.location), Print(s"$label = $value")


// Errors

sealed trait SyntaxErr(loc: Location) extends Located { def location = loc }
case class BadNumErr(lexeme: String, loc: Location) extends SyntaxErr(loc)
case class UnexpectedTokenErr[Expected](found: Token) extends SyntaxErr(found.location)
case class UnexpectedEofErr(prev: Located) extends SyntaxErr(prev.location)


// Token/expression location information

case class Location(source: String, offset: Int)
trait Located { def location: Location }
trait At(loc: Location) { def location = loc }


// Debugging and printing

trait Print(inner: String = ""):
  self =>
    override def toString =
      if inner == ""
      then getClass.getSimpleName.toUpperCase
      else inner
