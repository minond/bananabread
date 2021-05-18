package bananabread
package parsing.opcode

import parsing.{Parsed, takeWhile, not, isWhitespace, and, oneof, eat}
import parsing.location.{Location, Located, At}
import parsing.error.{SyntaxErr, UnexpectedTokenErr, UnexpectedEofErr, BadNumErr}
import utils.{isA, asList, squished, safeToInt}

import scala.reflect.ClassTag


case class Tree(nodes: List[Expr])


sealed trait Token extends Located
sealed trait Expr extends Located


case class OpenSquareBraket(loc: Location) extends Token, At(loc)
case class CloseSquareBraket(loc: Location) extends Token, At(loc)
case class Word(lexeme: String, loc: Location) extends Token, At(loc)
case class Comma(loc: Location) extends Token, At(loc)
case class Eof(loc: Location) extends Token, At(loc)


case class Label(label: String, loc: Location) extends Expr, At(loc)
case class Instruction(opcode: String, ty: Option[String] = None, args: List[String] = List.empty, loc: Location) extends Expr, At(loc)
case class Constant(label: String, ty: String, value: String, loc: Location) extends Expr, At(loc)


type Tokens = BufferedIterator[Token]

def parse(sourceName: String, sourceString: String): Parsed[Tree] =
  tokenize(sourceName, sourceString).flatMap { tokens =>
    parse(sourceName, tokens.iterator.buffered)
  }
def parse(sourceName: String, tokens: Tokens): Parsed[Tree] =
  for
    nodes <- tokens
      .map { (token) => parseTop(token, tokens) }
      .squished
  yield
    Tree(nodes)

def parseTop(head: Token, tail: Tokens): Parsed[Expr] = head match
  case op @ Word("add", _)     => parseOpcodeWithType(op, tail)
  case op @ Word("sub", _)     => parseOpcodeWithType(op, tail)
  case op @ Word("push", _)    => parseOpcodeWithTypeAndArg1(op, tail)
  case op @ Word("load", _)    => parseOpcodeWithTypeAndArg1(op, tail)
  case op @ Word("store", _)   => parseOpcodeWithTypeAndArg1(op, tail)
  case op @ Word("jz", _)      => parseOpcodeWithArg1(op, tail)
  case op @ Word("jmp", _)     => parseOpcodeWithArg1(op, tail)
  case op @ Word("call", _)    => parseOpcodeWithArg1(op, tail)
  case op @ Word("mov", _)     => parseMov(op, tail)
  case op @ Word("concat", _)  => parseOpcode(op, tail)
  case op @ Word("println", _) => parseOpcode(op, tail)
  case op @ Word("halt", _)    => parseOpcode(op, tail)
  case op @ Word("call0", _)   => parseOpcode(op, tail)
  case op @ Word("ret", _)     => parseOpcode(op, tail)
  case op @ Word("swap", _)    => parseOpcode(op, tail)
  case w: Word                 => parseLabel(w)
  case _: Eof                  => Left(UnexpectedEofErr(head))
  case _                       => Left(UnexpectedTokenErr(head))

def parseOpcode(op: Word, tail: Tokens): Parsed[Expr] =
    Right(Instruction(op.lexeme, None, List.empty, op.location))

def parseOpcodeWithType(op: Word, tail: Tokens): Parsed[Expr] =
    for
      _  <- eat[OpenSquareBraket](op, tail)
      ty <- eat[Word](op, tail)
      _  <- eat[CloseSquareBraket](op, tail)
    yield
      Instruction(op.lexeme, Some(ty.lexeme), List.empty, op.location)

def parseOpcodeWithArg1(op: Word, tail: Tokens): Parsed[Expr] =
    for
      arg1 <- eat[Word](op, tail)
    yield
      Instruction(op.lexeme, None, List(arg1.lexeme), op.location)

def parseOpcodeWithTypeAndArg1(op: Word, tail: Tokens): Parsed[Expr] =
    for
      _    <- eat[OpenSquareBraket](op, tail)
      ty   <- eat[Word](op, tail)
      _    <- eat[CloseSquareBraket](op, tail)
      name <- eat[Word](op, tail)
    yield
      Instruction(op.lexeme, Some(ty.lexeme), List(name.lexeme), op.location)

def parseMov(op: Word, tail: Tokens): Parsed[Expr] =
    for
      reg  <- eat[Word](op, tail)
      args <- if lookahead(op, tail).isA[Comma]
              then eat[Word](tail.next, tail).flatMap(parseNum).map(_.asList)
              else Right(List.empty)
    yield
      Instruction(op.lexeme, Some(reg.lexeme), args, op.location)

def parseLabel(label: Word): Parsed[Expr] =
  Right(Label(label.lexeme.dropRight(1), label.location))

def parseNum(word: Word): Parsed[String] =
  word.lexeme.safeToInt match
    case Left(_)  => Left(BadNumErr(word.lexeme, word.loc))
    case Right(_) => Right(word.lexeme)


def tokenize(sourceName: String, sourceString: String): Parsed[List[Token]] =
  tokenize(sourceName, sourceString.iterator.zipWithIndex.buffered)
def tokenize(sourceName: String, sourceStream: BufferedIterator[(Char, Int)]): Parsed[List[Token]] =
  sourceStream
    .filter { (c, _) => !c.isWhitespace }
    .map { (c, i) => nextToken(c, sourceStream, Location(sourceName, i)) }
    .squished

def nextToken(head: Char, tail: BufferedIterator[(Char, Int)], loc: Location): Parsed[Token] = head match
  case '[' => Right(OpenSquareBraket(loc))
  case ']' => Right(CloseSquareBraket(loc))
  case ',' => Right(Comma(loc))
  case _   => Right(Word(head +: takeWhile(tail, and(not(isWhitespace),
                                                     not(oneof('[', ']')))).mkString, loc))


def lookahead(head: Token, tail: Tokens): Token =
  tail.headOption match
    case Some(token) => token
    case None        => Eof(head.location)

def eat[T: ClassTag](head: Token, tail: Tokens): Parsed[T] = tail.headOption match
  case Some(token: T) =>
    tail.next
    Right(token)

  case Some(bad) => Left(UnexpectedTokenErr(bad))
  case None      => Left(UnexpectedEofErr(head))
