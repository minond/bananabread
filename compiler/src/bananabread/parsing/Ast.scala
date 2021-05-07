package bananabread
package parsing.ast

import utils.Print

import scala.reflect.ClassTag


// Base AST

case class Tree(nodes: List[Expr | Stmt])


sealed trait Token extends Located {
  def is[T : ClassTag] = this match
    case _ : T => true
    case _     => false
}

sealed trait Expr extends Located
sealed trait Stmt extends Located


// Tokens

case class Eof(loc: Location) extends Token, At(loc) with Print()
case class Comma(loc: Location) extends Token, At(loc) with Print()
case class Dot(loc: Location) extends Token, At(loc) with Print()
case class OpenParen(loc: Location) extends Token, At(loc) with Print()
case class CloseParen(loc: Location) extends Token, At(loc) with Print()
case class OpenCurlyParen(loc: Location) extends Token, At(loc) with Print()
case class CloseCurlyParen(loc: Location) extends Token, At(loc) with Print()
case class OpenSquareBraket(loc: Location) extends Token, At(loc) with Print()
case class CloseSquareBraket(loc: Location) extends Token, At(loc) with Print()


// Aux tokens
case class Comment(lexeme: String, loc: Location) extends Token, At(loc)


// Token expressions

case class Num(lexeme: String, loc: Location) extends Token, At(loc) with Expr, Print(lexeme)
case class Str(lexeme: String, loc: Location) extends Token, At(loc) with Expr, Print(s""""$lexeme"""")
case class Id(lexeme: String, loc: Location) extends Token, At(loc) with Expr, Print(lexeme)
case class Symbol(lexeme: String, loc: Location) extends Token, At(loc) with Expr, Print(s"'$lexeme")


// Expressions

case class Binop(op: Id, lhs: Expr, rhs: Expr) extends Expr with At(op.location), Print(s"($op $lhs $rhs)")
case class Uniop(op: Id, operand: Expr) extends Expr with At(op.location), Print(s"($op $operand)")
case class App(lambda: Expr, args: List[Expr]) extends Expr with At(lambda.location), Print(s"(${(lambda +: args).mkString(" ")})")
case class Lambda(params: List[Expr], body: Expr) extends Expr with At(body.location), Print(s"{${params.mkString(", ")} = $body}")
case class Cond(start: Token, cond: Expr, pass: Expr, fail: Expr) extends Expr with At(start.location), Print(s"if $cond then $pass else $fail")
case class Let(start: Token, bindings: List[Binding], body: Expr) extends Expr with At(start.location), Print(s"let ${bindings.mkString(" ")} in $body")
case class Begin(head: Expr, tail: List[Expr]) extends Expr with At(head.location), Print(s"begin ${(head +: tail).mkString(" ")} end")


// Aux expressions
case class Binding(label: Id, value: Expr) extends At(label.location), Print(s"$label = $value")


// Statements

case class Def(name: Id, value: Expr) extends Stmt, At(name.location), Print(s"def $name = $value")


// Errors

sealed trait SyntaxErr(loc: Location) extends Located { def location = loc }
case class BadNumErr(lexeme: String, loc: Location) extends SyntaxErr(loc)
case class EmptyBeginNotAllowedErr(start: Token) extends SyntaxErr(start.location)
case class UnexpectedTokenErr[Expected](found: Token) extends SyntaxErr(found.location)
case class UnexpectedEofErr(prev: Located) extends SyntaxErr(prev.location)
case class UnclosedStringErr(loc: Location) extends SyntaxErr(loc)


// Token/expression location information

case class Location(source: String, offset: Int)
trait Located { def location: Location }
trait At(loc: Location) { def location = loc }
