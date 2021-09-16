package bananabread
package parsing
package opcode

import ast._
import error._
import location._

import utils.{Print, isA, asList, squished, safeToInt}

import scala.reflect.ClassTag


sealed trait Expr extends Located
case class Label(label: Id) extends Expr, At(label.location)
case class Instruction(opcode: Id, ty: Option[Id] = None, args: List[Token] = List.empty) extends Expr, At(opcode.location)


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
      Instruction(op, Some(ty), List.empty)

def parseOpcodeInstructionWithTypeAndArg1(op: Id, tail: Tokens): Parsed[Expr] =
    for
      ob   <- eat[ast.OpenSquareBraket](tail.next, tail)
      ty   <- eat[Id](ob, tail)
      cb   <- eat[ast.CloseSquareBraket](ty, tail)
      name <- eat[Id](cb, tail)
    yield
      Instruction(op, Some(ty), List(name))

def parseOpcodeInstructionWord(op: Id, tail: Tokens): Parsed[Expr] =
  tail.next
  Right(Instruction(op, None, List.empty))

def parseOpcodeInstructionWithOneArg(op: Id, tail: Tokens): Parsed[Expr] =
  next(tail.next, tail) match
    case label: Id  => Right(Instruction(op, None, List(label)))
    case unexpected => Left(UnexpectedTokenErr(unexpected))

def parseOpcodeLabel(label: Id, tail: Tokens): Parsed[Expr] =
  next(tail.next, tail) match
    case _: Colon => Right(Label(label))
    case unexpected => Left(MissingExpectedTokenAfterErr(label, unexpected, Expected.Colon))
