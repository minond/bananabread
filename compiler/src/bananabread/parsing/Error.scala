package bananabread
package parsing
package error

import location.{Location, Located}
import utils.Print


sealed trait SyntaxErr(loc: Location) extends Located { def location = loc }
case class BadNumErr(lexeme: String, loc: Location) extends SyntaxErr(loc)
case class EmptyBeginNotAllowedErr(start: Located) extends SyntaxErr(start.location)
case class UnexpectedTokenErr[Expected](found: Located) extends SyntaxErr(found.location)
case class UnexpectedEofErr(prev: Located) extends SyntaxErr(prev.location)
case class UnclosedStringErr(loc: Location) extends SyntaxErr(loc)
case class BadOperatorDefinitionErr(loc: Location) extends SyntaxErr(loc)

enum Expected:
  case Colon extends Expected with Print(":")

case class MissingExpectedTokenErr(found: Located, expected: Expected) extends SyntaxErr(found.location)
case class MissingExpectedTokenAfterErr(after: Located, found: Located, expected: Expected) extends SyntaxErr(found.location)
