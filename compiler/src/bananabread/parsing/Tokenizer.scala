package bananabread
package parsing

import ast._
import error._
import location.Location

import utils.{squished, safeToFloat}


def tokenize(sourceName: String, sourceString: String, syntax: Syntax): Parsed[List[Token]] =
  tokenize(sourceName, sourceString.iterator.zipWithIndex.buffered, syntax)
def tokenize(sourceName: String, sourceStream: BufferedIterator[(Char, Int)], syntax: Syntax): Parsed[List[Token]] =
  sourceStream
    .filter { (c, _) => !c.isWhitespace }
    .map { (c, i) => nextToken(c, sourceStream, Location(sourceName, i), syntax) }
    .squished


def nextToken(
  head: Char,
  tail: BufferedIterator[(Char, Int)],
  loc: Location,
  syntax: Syntax,
  ignoreComment: Boolean = false,
  ignorePString: Boolean = false,
): Parsed[Token] = head match
  case ',' => Right(Comma(loc))
  case '.' => Right(Dot(loc))
  case ':' => Right(Colon(loc))
  case '(' => Right(OpenParen(loc))
  case ')' => Right(CloseParen(loc))
  case '{' => Right(OpenCurlyParen(loc))
  case '}' => Right(CloseCurlyParen(loc))
  case '[' => Right(OpenSquareBraket(loc))
  case ']' => Right(CloseSquareBraket(loc))

  case '/' if !ignoreComment =>
    tail.headOption match
      case Some('/', _) =>
        val comment = takeWhile(skip(tail), not(isNewline))
        Right(Comment(comment.mkString.strip, loc))
      case _ => nextToken(head, tail, loc, syntax, ignoreComment=true)

  case '%' if !ignorePString =>
    tail.headOption match
      case Some('{', _) =>
        val str = takeWhile(skip(tail), aint('}'))
        if tail.isEmpty || tail.next._1 != '}'
        then Left(UnclosedStringErr(loc))
        else Right(Str(str.mkString, loc))
      case Some('[', _) =>
        Right(Percent(loc))
      case _ => nextToken(head, tail, loc, syntax, ignorePString=true)

  case '"' =>
    val str = takeWhile(tail, aint('"'))
    if tail.isEmpty || tail.next._1 != '"'
    then Left(UnclosedStringErr(loc))
    else Right(Str(str.mkString, loc))

  case '\'' =>
    val symbol = takeWhile(tail, isSymbolTail).mkString

    Right(Symbol(symbol, loc))

  case head if isNumHead(head) =>
    val rest = takeWhile(tail, isNumTail).mkString
    val lexeme = head +: rest

    lexeme.safeToFloat match
      case Left(_)  => Left(BadNumErr(lexeme, loc))
      case Right(_) => Right(Num(lexeme, loc))

  case head if isIdHead(head) =>
    val rest = takeWhile(tail, isIdTail).mkString
    val lexeme = head +: rest

    lexeme match
      case "true"  => Right(True(loc))
      case "false" => Right(False(loc))
      case id      => Right(Id(lexeme, loc))

  case head =>
    val rest = takeWhile(tail, isUnknownTail).mkString
    val lexeme = head +: rest

    Right(Id(lexeme, loc))


val isNumHead = isNumeric
val isNumTail = or(isNumHead,
                   is('.'))
val isIdTail = and(not(isWhitespace),
                   or(isNumeric,
                      isLetter,
                      is('_')))
val isIdHead = and(isIdTail,
                   not(isNumeric))
val isUnknownTail = and(not(isIdTail),
                        not(isWhitespace),
                        not(oneof(',', '.', '(', ')', '{', '}', '[', ']')))
val isSymbolTail = and(not(isWhitespace),
                       not(oneof(',', '(', ')', '{', '}', '[', ']')))
