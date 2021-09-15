package bananabread
package parsing
package opcode

import ast._
import error._
import location._

import utils.{Print, isA, asList, squished, safeToInt}

import scala.reflect.ClassTag


sealed trait Expr extends Located
case class Label(label: String, loc: Location) extends Expr, At(loc)
case class Instruction(opcode: String, ty: Option[String] = None, args: List[String] = List.empty, loc: Location) extends Expr, At(loc)


def parseOpcodeInstruction(head: Token, tail: Tokens): Parsed[Expr] =
  lookahead(head, tail) match
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

def parseOpcodeInstructionWithType(op: Id, tail: Tokens): Parsed[Expr] =
    for
      ob <- eat[ast.OpenSquareBraket](tail.next, tail)
      ty <- eat[Id](ob, tail)
      _  <- eat[ast.CloseSquareBraket](ty, tail)
    yield
      Instruction(op.lexeme, Some(ty.lexeme), List.empty, op.location)

def parseOpcodeInstructionWithTypeAndArg1(op: Id, tail: Tokens): Parsed[Expr] =
    for
      ob   <- eat[ast.OpenSquareBraket](tail.next, tail)
      ty   <- eat[Id](ob, tail)
      cb   <- eat[ast.CloseSquareBraket](ty, tail)
      name <- eat[Id](cb, tail)
    yield
      Instruction(op.lexeme, Some(ty.lexeme), List(name.lexeme), op.location)

def parseOpcodeInstructionWord(op: Id, tail: Tokens): Parsed[Expr] =
  tail.next
  Right(Instruction(op.lexeme, None, List.empty, op.location))

def parseOpcodeInstructionWithOneArg(op: Id, tail: Tokens): Parsed[Expr] =
  next(tail.next, tail) match
    case Id(label, _) => Right(Instruction(op.lexeme, None, List(label), op.location))
    case unexpected   => Left(UnexpectedTokenErr(unexpected))

def parseOpcodeLabel(label: Id, tail: Tokens): Parsed[Expr] =
  next(tail.next, tail) match
    case _: Colon => Right(Label(label.lexeme, label.location))
    case unexpected => Left(MissingExpectedTokenAfterErr(label, unexpected, Expected.Colon))
