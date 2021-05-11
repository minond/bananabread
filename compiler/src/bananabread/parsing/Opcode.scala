package bananabread
package parsing.opcode

import utils.EitherImplicits
import parsing.lang.{takeWhile, not, isWhitespace, and, oneof, eat}

import scala.reflect.ClassTag


case class Tree(nodes: List[Expr])


sealed trait Expr
case class Label(label: String) extends Expr
case class Instruction(opcode: String, ty: Option[String] = None, args: List[String] = List.empty) extends Expr
case class Constant(label: String, ty: String, value: String) extends Expr


sealed trait Token
case object OpenSquareBraket extends Token
case object CloseSquareBraket extends Token
case class Word(lexeme: String) extends Token


sealed trait Err
case class UnexpectedTokenErr(token: Token) extends Err
case object UnexpectedEofErr extends Err


type Tokens = BufferedIterator[Token]

def parse(sourceString: String): Either[Err, Tree] =
  tokenize(sourceString).flatMap { tokens => parse(tokens.iterator.buffered) }
def parse(tokens: Tokens): Either[Err, Tree] =
  for
    nodes <- tokens
      .map { (token) => parseTop(token, tokens) }
      .squished
  yield
    Tree(nodes)

def parseTop(head: Token, tail: Tokens): Either[Err, Expr] = head match
  case op @ Word("add") => parseOpcodeWithType(op, tail)
  case op @ Word("load") => parseOpcodeWithTypeAndArg1(op, tail)
  case op @ Word("ret") => parseOpcode(op, tail)
  case _ => Left(UnexpectedTokenErr(head))

def parseOpcode(op: Word, tail: Tokens): Either[Err, Expr] =
    Right(Instruction(op.lexeme))

def parseOpcodeWithType(op: Word, tail: Tokens): Either[Err, Expr] =
    for
      _  <- eat[OpenSquareBraket.type](op, tail)
      ty <- eat[Word](op, tail)
      _  <- eat[CloseSquareBraket.type](op, tail)
    yield
      Instruction(op.lexeme, Some(ty.lexeme))

def parseOpcodeWithTypeAndArg1(op: Word, tail: Tokens): Either[Err, Expr] =
    for
      _    <- eat[OpenSquareBraket.type](op, tail)
      ty   <- eat[Word](op, tail)
      _    <- eat[CloseSquareBraket.type](op, tail)
      name <- eat[Word](op, tail)
    yield
      Instruction(op.lexeme, Some(ty.lexeme), List(name.lexeme))

def eat[T: ClassTag](head: Token, tail: Tokens): Either[Err, T] = tail.headOption match
  case Some(token: T) =>
    tail.next
    Right(token)

  case Some(bad) => Left(UnexpectedTokenErr(bad))
  case None      => Left(UnexpectedEofErr)


def tokenize(sourceString: String): Either[Err, List[Token]] =
  tokenize(sourceString.iterator.zipWithIndex.buffered)
def tokenize(sourceStream: BufferedIterator[(Char, Int)]): Either[Err, List[Token]] =
  sourceStream
    .filter { (c, _) => !c.isWhitespace }
    .map { (c, i) => nextToken(c, sourceStream) }
    .squished

def nextToken(head: Char, tail: BufferedIterator[(Char, Int)]): Either[Err, Token] = head match
  case '[' => Right(OpenSquareBraket)
  case ']' => Right(CloseSquareBraket)
  case _   => Right(Word(head +: takeWhile(tail, and(not(isWhitespace), not(oneof('[', ']')))).mkString))
