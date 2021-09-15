package bananabread
package parsing
package opcode

import ast.{Id, Colon}
import location.{Location, Located, At}
import error.{SyntaxErr, UnexpectedTokenErr, UnexpectedEofErr, BadNumErr}
import utils.{Print, isA, asList, squished, safeToInt}

import scala.reflect.ClassTag


case class Tree(nodes: List[Expr])


sealed trait Token extends Located
sealed trait Expr extends Located


case class OpenSquareBraket(loc: Location) extends Token, At(loc) with Print("[")
case class CloseSquareBraket(loc: Location) extends Token, At(loc) with Print("]")
case class Word(lexeme: String, loc: Location) extends Token, At(loc) with Print(lexeme)
case class Comma(loc: Location) extends Token, At(loc) with Print(",")
case class Eof(loc: Location) extends Token, At(loc) with Print("<eof>")


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
  case op @ Word("stw", _)     => parseOpcodeWithArg1(op, tail)
  case op @ Word("ldw", _)     => parseOpcodeWithArg1(op, tail)
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



def parseOpcodeInstruction(head: ast.Token, tail: language.Tokens): Parsed[Expr] =
  parsing.lookahead(head, tail) match
    case op @ Id("add", _)     => parseOpcodeInstructionWithType(op, tail)
    case op @ Id("sub", _)     => parseOpcodeInstructionWithType(op, tail)
    case op @ Id("push", _)    => parseOpcodeInstructionWithTypeAndArg1(op, tail)
    case op @ Id("load", _)    => parseOpcodeInstructionWithTypeAndArg1(op, tail)
    case op @ Id("store", _)   => parseOpcodeInstructionWithTypeAndArg1(op, tail)
    case op @ Id("jz", _)      => parseOpcodeInstructionWithOneArg(op, tail)
    case op @ Id("jmp", _)     => parseOpcodeInstructionWithOneArg(op, tail)
    case op @ Id("call", _)    => parseOpcodeInstructionWithOneArg(op, tail)
    case op @ Id("stw", _)     => parseOpcodeInstructionWithOneArg(op, tail)
    case op @ Id("ldw", _)     => parseOpcodeInstructionWithOneArg(op, tail)
    // case op @ Id("mov", _)     => parseMov(op, tail)
    case op @ Id("concat", _)  => parseOpcodeInstructionWord(op, tail)
    case op @ Id("println", _) => parseOpcodeInstructionWord(op, tail)
    case op @ Id("halt", _)    => parseOpcodeInstructionWord(op, tail)
    case op @ Id("call0", _)   => parseOpcodeInstructionWord(op, tail)
    case op @ Id("ret", _)     => parseOpcodeInstructionWord(op, tail)
    case op @ Id("swap", _)    => parseOpcodeInstructionWord(op, tail)
    case label: Id             => parseOpcodeLabel(label, tail)
    case unexpected            => Left(UnexpectedTokenErr(unexpected))

def parseOpcodeInstructionWithType(op: Id, tail: language.Tokens): Parsed[Expr] =
    for
      ob <- parsing.eat[ast.OpenSquareBraket](tail.next, tail)
      ty <- parsing.eat[Id](ob, tail)
      _  <- parsing.eat[ast.CloseSquareBraket](ty, tail)
    yield
      Instruction(op.lexeme, Some(ty.lexeme), List.empty, op.location)

def parseOpcodeInstructionWithTypeAndArg1(op: Id, tail: language.Tokens): Parsed[Expr] =
    for
      ob   <- parsing.eat[ast.OpenSquareBraket](tail.next, tail)
      ty   <- parsing.eat[Id](ob, tail)
      cb   <- parsing.eat[ast.CloseSquareBraket](ty, tail)
      name <- parsing.eat[Id](cb, tail)
    yield
      Instruction(op.lexeme, Some(ty.lexeme), List(name.lexeme), op.location)

def parseOpcodeInstructionWord(op: Id, tail: language.Tokens): Parsed[Expr] =
  tail.next
  Right(Instruction(op.lexeme, None, List.empty, op.location))

def parseOpcodeInstructionWithOneArg(op: Id, tail: language.Tokens): Parsed[Expr] =
  next(tail.next, tail) match
    case Id(label, _) => Right(Instruction(op.lexeme, None, List(label), op.location))
    case unexpected   => Left(UnexpectedTokenErr(unexpected))

def parseOpcodeLabel(label: Id, tail: language.Tokens): Parsed[Expr] =
  next(tail.next, tail) match
    case _: Colon => Right(Label(label.lexeme, label.location))
    case unexpected => Left(UnexpectedTokenErr[Colon](unexpected))
