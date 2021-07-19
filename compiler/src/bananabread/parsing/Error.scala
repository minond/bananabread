package bananabread
package parsing.error

import parsing.location.{Location, Located}


sealed trait SyntaxErr(loc: Location) extends Located { def location = loc }
case class BadNumErr(lexeme: String, loc: Location) extends SyntaxErr(loc)
case class EmptyBeginNotAllowedErr(start: Located) extends SyntaxErr(start.location)
case class UnexpectedTokenErr[Expected](found: Located) extends SyntaxErr(found.location)
case class UnexpectedEofErr(prev: Located) extends SyntaxErr(prev.location)
case class UnclosedStringErr(loc: Location) extends SyntaxErr(loc)
case class BadOperatorDefinitionErr(loc: Location) extends SyntaxErr(loc)
