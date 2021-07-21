package bananabread
package parsing.ast

import parsing.location.{Location, Located, At}
import utils.Print


case class Tree(val nodes: List[Expr | Stmt])


sealed trait Token extends Located
sealed trait Expr extends Located
sealed trait Stmt extends Located


case class Eof(loc: Location) extends Token, At(loc) with Print("<EOF>")
case class Comma(loc: Location) extends Token, At(loc) with Print(",")
case class Dot(loc: Location) extends Token, At(loc) with Print(".")
case class Colon(loc: Location) extends Token, At(loc) with Print(":")
case class OpenParen(loc: Location) extends Token, At(loc) with Print("(")
case class CloseParen(loc: Location) extends Token, At(loc) with Print(")")
case class OpenCurlyParen(loc: Location) extends Token, At(loc) with Print("{")
case class CloseCurlyParen(loc: Location) extends Token, At(loc) with Print("}")
case class OpenSquareBraket(loc: Location) extends Token, At(loc) with Print("[")
case class CloseSquareBraket(loc: Location) extends Token, At(loc) with Print("]")


case class Num(lexeme: String, loc: Location) extends Token, At(loc) with Expr, Print(lexeme)
case class Str(lexeme: String, loc: Location) extends Token, At(loc) with Expr, Print(s""""$lexeme"""")
case class Id(lexeme: String, loc: Location) extends Token, At(loc) with Expr, Print(lexeme)
case class Symbol(lexeme: String, loc: Location) extends Token, At(loc) with Expr, Print(s"'$lexeme")


case class Binop(op: Id, lhs: Expr, rhs: Expr) extends Expr with At(op.location), Print(s"($op $lhs $rhs)")
case class Uniop(op: Id, operand: Expr) extends Expr with At(op.location), Print(s"($op $operand)")
case class App(lambda: Expr, args: List[Expr]) extends Expr with At(lambda.location), Print(s"(${(lambda +: args).mkString(" ")})")
case class Cond(start: Token, cond: Expr, pass: Expr, fail: Expr) extends Expr with At(start.location), Print(s"if $cond then $pass else $fail")
case class Let(start: Token, bindings: List[Binding], body: Expr) extends Expr with At(start.location), Print(s"let ${bindings.mkString(" ")} in $body")
case class Begin(head: Expr, tail: List[Expr]) extends Expr with At(head.location), Print(s"begin ${(head +: tail).mkString(" ")} end")


case class Lambda(params: List[Param], body: Expr, tyVars: List[Ty], tyRet: Option[Ty]) extends Expr with At(body.location), Print(s"{${params.mkString(", ")} = ${body}${ppTy(tyRet)}}")
case class Param(name: Id, ty: Option[Ty]) extends Token, At(name.location), Print(name.toString + ppTy(ty))
case class Ty(ty: Id)


case class Comment(lexeme: String, loc: Location) extends Token, At(loc)
case class Binding(label: Id, value: Expr) extends At(label.location), Print(s"$label = $value")
case class Def(name: Id, value: Expr) extends Stmt, At(name.location), Print(s"def $name = $value")


def ppTy(ty: Option[Ty]): String =
  ty.map(_.ty).map(" : " + _).getOrElse("")
