package bananabread
package error

import parsing.error => parse
import parsing.location.Location

import scala.io.AnsiColor.{BOLD, RESET}


def pp(err: parse.SyntaxErr, source: String) = err match
  case parse.UnexpectedTokenErr(token) =>
    List(
      generateSyntaxErrorLine(s"unexpected ${token.getClass.getSimpleName} found", token.location, source),
      isolateBadLine(token.location, source),
    ).mkString("\n")

  case _ =>
    err.toString

def generateSyntaxErrorLine(message: String, loc: Location, source: String) =
  s"${BOLD}syntax error, ${message} in ${generateCoordinates(loc, source)}${RESET}"

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
  val sourcePadding = 3
  val bads = lines.zipWithIndex.slice(row - sourcePadding, row + sourcePadding)
  bads.foldLeft[List[String]](List.empty) {
    case (acc, (line, index)) =>
      if index == row
      then acc :+ withLineNumber(max, index, line) :+ withLineNumber(max, -1, pointer)
      else acc :+ withLineNumber(max, index, line)
  }.mkString("\n")

def generatePointer(col: Int) =
  " " * col + "^"

def withLineNumber(max: Int, row: Int, line: String) =
  if row == -1
  then s"${pad(max, "-", "-")}|$line"
  else s"${pad(max, (row + 1).toString, "0")}|$line"

def pad(max: Int, str: String, using: String) =
  val currSize = str.size
  val maxSize = max.toString.size
  if currSize < maxSize
  then (using * (maxSize - currSize)) + str
  else str
