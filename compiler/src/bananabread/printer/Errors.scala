package bananabread
package printer
package errors

import ir.typed
import ir.linked
import parsing.ast
import parsing.location.Location
import runtime.instruction

import error.Err
import backend.opcode.error => genopErr
import parsing.error => parseErr
import runtime.error => runtimeErr
import typechecker.error => typecheckerErr
import ir.typeless.error => typelessIrErr
import ir.typed.error => typedIrErr
import ir.linked.error => linkedIrErr

import scala.io.AnsiColor.{BOLD, RESET}


val GRAY = String("\u001B[37m")

val SourcePadding = 5
val OpcodePadding = 10


def pp(err: Err, source: String): String = err match
  case parseErr.BadNumErr(_, location) =>
    lines(
      generateSyntaxErrorLine(s"bad number", location, source),
      isolateBadLine(location, source),
    )

  case parseErr.UnclosedStringErr(location) =>
    lines(
      generateSyntaxErrorLine(s"bad string", location, source),
      isolateBadLine(location, source),
    )

  case parseErr.EmptyBeginNotAllowedErr(token) =>
    lines(
      generateSyntaxErrorLine(s"empty begin", token.location, source),
      isolateBadLine(token.location, source),
    )

  case parseErr.BadOperatorDefinitionErr(location) =>
    lines(
      generateSyntaxErrorLine(s"bad operator definition", location, source),
      isolateBadLine(location, source),
    )

  case parseErr.UnexpectedEofErr(prev) =>
    lines(
      generateSyntaxErrorLine(s"unexpected <eof> after `$prev`", prev.location, source),
      isolateBadLine(prev.location, source),
    )

  case parseErr.UnexpectedTokenErr(token) =>
    lines(
      generateSyntaxErrorLine(s"unexpected token `$token` found", token.location, source),
      isolateBadLine(token.location, source),
    )

  case parseErr.MissingExpectedTokenErr(token, expected) =>
    lines(
      generateSyntaxErrorLine(s"expected `$expected` but found `$token`", token.location, source),
      isolateBadLine(token.location, source),
    )

  case parseErr.MissingExpectedTokenAfterErr(after, token, expected) =>
    lines(
      generateSyntaxErrorLine(s"expected `$expected` after `$after` but found `$token`", token.location, source),
      isolateBadLine(after.location, source),
    )

  case genopErr.BadPushErr(ty, node) =>
    node.expr match
      case expr: ast.Expr =>
        lines(
          generateAnalysisErrorLine(Step.Compile, s"bad push, `${expr}` of type `${ty}` is not a valid runtime value", expr.location, source),
          isolateBadLine(expr.location, source),
        )
      case stmt: ast.Def =>
        lines(
          generateAnalysisErrorLine(Step.Compile, s"bad push, `${stmt.value}` is not a valid runtime value", stmt.value.location, source),
          isolateBadLine(stmt.value.location, source),
        )
      case node: (ast.Module | ast.Import) =>
        lines(
          generateInternalErrorLine(s"unable to process code", node.location, source),
          isolateBadLine(node.location, source),
        )

  case genopErr.UndeclaredIdentifierErr(id) =>
    lines(
      generateAnalysisErrorLine(Step.Compile, s"${id.expr.lexeme} was referenced but not found", id.expr.location, source),
      isolateBadLine(id.expr.location, source),
    )

  case genopErr.LookupErr(id) =>
    lines(
      generateAnalysisErrorLine(Step.Compile, s"${id.lexeme} was referenced but not found", id.location, source),
      isolateBadLine(id.location, source),
    )

  case genopErr.UnknownUserOpcodeErr(code, loc) =>
    lines(
      generateSyntaxErrorLine(s"invalid opcode", loc, source),
      isolateBadLine(code.location, source),
    )

  case genopErr.BadCallErr(typed.Id(id, _, _)) =>
    lines(
      generateAnalysisErrorLine(Step.Compile, s"bad call to `${id.lexeme}`", id.location, source),
      isolateBadLine(id.location, source),
    )

  case genopErr.BadCallErr(node) =>
    lines(
      generateAnalysisErrorLine(Step.Compile, s"bad call to value of kind ${node.getClass.getSimpleName}", node.expr.location, source),
      isolateBadLine(node.expr.location, source),
    )

  case runtimeErr.RuntimeErr(msg, ins, codes, registers) =>
    lines(
      generateVmRuntimeErrorLine(msg),
      isolateBadOpcode(registers.pc.value, codes),
    )

  case typecheckerErr.UnknowTypeErr(tag) =>
    lines(
      generateTypeErrorLine(s"unknown type `${tag.id.lexeme}`", tag.id.location, source),
      isolateBadLine(tag.id.location, source),
    )

  case typecheckerErr.LookupErr(label, ir) =>
    lines(
      generateTypeErrorLine(s"`${label}` was referenced but not found", ir.expr.location, source),
      isolateBadLine(ir.expr.location, source),
    )

  case typecheckerErr.UnunifiedTypeVarErr(_, ir) =>
    lines(
      generateTypeErrorLine(s"unable to unify expression, perhaps a type annotation is missing", ir.expr.location, source),
      isolateBadLine(ir.expr.location, source),
    )

  case typecheckerErr.UnificationErr(expected, got, ir) =>
    lines(
      generateTypeErrorLine(s"expected value of type `$expected` but got value of type `$got`", ir.expr.location, source),
      isolateBadLine(ir.expr.location, source),
    )

  case typecheckerErr.TypeMismatchErr(expected, got, ir) =>
    lines(
      generateTypeErrorLine(s"expected value of type `$expected` but got value of type `$got`", ir.expr.location, source),
      isolateBadLine(ir.expr.location, source),
    )

  case typecheckerErr.GenTypeMismatchErr(expected, got, ir) =>
    lines(
      generateTypeErrorLine(s"expected value of type `${expected.getSimpleName}` but got value of type `$got`", ir.expr.location, source),
      isolateBadLine(ir.expr.location, source),
    )

  case typelessIrErr.LiftinTheUnliftableErr(node) =>
    lines(
      generateInternalErrorLine(s"unable to process code", node.location, source),
      isolateBadLine(node.location, source),
    )

  case typedIrErr.UndeclaredIdentifierErr(linked.Id(id, _)) =>
    lines(
      generateTypeErrorLine(s"undeclared identifier `$id`", id.location, source),
      isolateBadLine(id.location, source),
    )

  case linkedIrErr.UndeclaredIdentifierErr(node) =>
    lines(
      generateAnalysisErrorLine(Step.Linked, s"${node.expr.lexeme} was referenced but not found", node.expr.location, source),
      isolateBadLine(node.expr.location, source),
    )

  case _ =>
    s"${err.getClass.getCanonicalName}: ${err}"


enum Step:
  case Typeless
  case Linked
  case Typed
  case Stitched
  case Compile

  override def toString = this match
    case Typeless => "tl"
    case Linked => "linked"
    case Typed => "typed"
    case Stitched => "stitched"
    case Compile => "compile"

def generateSyntaxErrorLine(message: String, loc: Location, source: String) =
  generateErrorLine("syntax error", message, loc, source)

def generateAnalysisErrorLine(step: Step, message: String, loc: Location, source: String) =
  generateErrorLine("analysis error", s"${message} [${step}]", loc, source)

def generateVmRuntimeErrorLine(message: String) =
  s"${BOLD}runtime error: ${message}${RESET}"

def generateTypeErrorLine(message: String, loc: Location, source: String) =
  generateErrorLine("type error", message, loc, source)

def generateInternalErrorLine(message: String, loc: Location, source: String) =
  generateErrorLine("internal error", message, loc, source)

def generateErrorLine(errtype: String, message: String, loc: Location, source: String) =
  s"${BOLD}${generateCoordinates(loc, source)}: ${errtype}: ${message}${RESET}"


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

def isolateBadOpcode(pc: Int, codes: List[instruction.Code]) =
  val max = codes.size
  val bads = codes.zipWithIndex.slice(pc - OpcodePadding, pc + OpcodePadding)
  val pointer = generatePointer(0)
  bads.foldLeft[List[String]](List.empty) {
    case (acc, (line, index)) =>
      val code = line match
        case op: instruction.Data        => instructions.pp(op)
        case op: instruction.Label       => s"    ${instructions.pp(op)}"
        case op: instruction.Instruction => s"        ${instructions.pp(op)}"
      val formatted =
        if index == pc
        then s"${BOLD}${code}${RESET}"
        else code

      acc :+ withLineNumber(max, index, formatted)
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
