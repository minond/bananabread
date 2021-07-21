package bananabread
package error

import backend.opcode.error => genop
import parsing.error => parse
import runtime.error => runtime

import ir.typeless => tl
import parsing.location.Location
import bananabread.runtime.instruction.{Instruction, Code, pp => inspp}

import scala.io.AnsiColor.{BOLD, RESET}

val GRAY = String("\u001B[37m")

val SourcePadding = 5
val OpcodePadding = 10


type Errors = parse.SyntaxErr
            | genop.GeneratorErr
            | runtime.RuntimeErr


def pp(err: Errors, source: String) = err match
  case parse.UnexpectedEofErr(prev) =>
    lines(
      generateSyntaxErrorLine(s"unexpected <EOF> after `$prev`", prev.location, source),
      isolateBadLine(prev.location, source),
    )

  case parse.UnexpectedTokenErr(token) =>
    lines(
      generateSyntaxErrorLine(s"unexpected token `$token` found", token.location, source),
      isolateBadLine(token.location, source),
    )

  case genop.LookupErr(id) =>
    lines(
      generateRuntimeErrorLine(s"${id.id.lexeme} was referenced but not found", id.id.location, source),
      isolateBadLine(id.id.location, source),
    )

  case genop.UnknownUserOpcodeErr(code, source, loc) =>
    lines(
      generateSyntaxErrorLine(s"invalid opcode", loc, source),
      isolateBadLine(code.location, source),
    )

  case genop.BadCallErr(tl.Id(id)) =>
    lines(
      generateRuntimeErrorLine(s"bad call to ${id.lexeme}", id.location, source),
      isolateBadLine(id.location, source),
    )

  case runtime.FatalErr(msg, ins, codes, registers) =>
    lines(
      generateOpcodeErrorLine(msg),
      isolateBadOpcode(registers.pc.value, codes),
    )

  case _ =>
    err.toString

def generateSyntaxErrorLine(message: String, loc: Location, source: String) =
  s"${BOLD}syntax error: ${message} in ${generateCoordinates(loc, source)}${RESET}"

def generateRuntimeErrorLine(message: String, loc: Location, source: String) =
  s"${BOLD}runtime error: ${message} in ${generateCoordinates(loc, source)}${RESET}"

def generateOpcodeErrorLine(message: String) =
  s"${BOLD}opcode runtime error: ${message}${RESET}"

def generateCoordinates(loc: Location, source: String) =
  val (row, col) = offsetToRowAndCol(loc.offset, source)
  s"${loc.source}:${row + 1}:${col + 1}"

def offsetToRowAndCol(offset: Int, source: String): (Int, Int) =
  source.split("\n").foldLeft[(Int, Int)]((0, offset)) {
    case ((row, rem), line) =>
      if rem - line.size <= 0
      then return (row, rem)
      else (row + 1, rem - line.size - 1)
  }

def isolateBadLine(loc: Location, source: String) =
  val (row, col) = offsetToRowAndCol(loc.offset, source)
  val lines = source.split("\n")
  val max = lines.size
  val pointer = generatePointer(col)
  val bads = lines.zipWithIndex.slice(row - SourcePadding, row + SourcePadding)
  bads.foldLeft[List[String]](List.empty) {
    case (acc, (line, index)) =>
      if index == row
      then acc :+ withLineNumber(max, index, line) :+ withLineNumber(max, -1, pointer)
      else acc :+ withLineNumber(max, index, line)
  }.mkString("\n")

def isolateBadOpcode(pc: Int, codes: List[Code]) =
  val max = codes.size
  val bads = codes.zipWithIndex.slice(pc - OpcodePadding, pc + OpcodePadding)
  val pointer = generatePointer(0)
  bads.foldLeft[List[String]](List.empty) {
    case (acc, (line, index)) =>
      if index == pc
      then acc :+ withLineNumber(max, index, inspp(line)) :+ withLineNumber(max, -1, pointer)
      else acc :+ withLineNumber(max, index, inspp(line))
  }.mkString("\n")

def generatePointer(col: Int) =
  " " * col + s"${BOLD}^${RESET}"

def withLineNumber(max: Int, row: Int, line: String) =
  if row == -1
  then s"${GRAY}${pad(max, " ", " ")}|${RESET} $line"
  else s"${GRAY}${pad(max, (row + 1).toString, "0")}|${RESET} $line"

def pad(max: Int, str: String, using: String) =
  val currSize = str.size
  val maxSize = max.toString.size
  if currSize < maxSize
  then (using * (maxSize - currSize)) + str
  else str

def lines(args: String*): String =
  args.mkString("\n")
