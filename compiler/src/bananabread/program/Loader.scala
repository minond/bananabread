package bananabread
package program

import parsing.ast
import ir.{typed, typeless}
import ir.typed.{Ir, Lifted}
import error.Err
import utils.squished

import scala.io.Source


def load(ref: ast.Ref): Either[Err, List[SourceFile]] =
  load(refFileName(ref))
def load(stmt: ast.Import): Either[Err, List[SourceFile]] =
  load(stmt.name)
def load(fileName: String): Either[Err, List[SourceFile]] =
  load(fileName, loadSource(fileName))
def load(fileName: String, code: String): Either[Err, List[SourceFile]] =
  for
    nodes <- parsing.language.parse(fileName, code)
    (module, imports, tree) = parsing.program.structure(nodes)
    rest <- imports.map(load).squished
  yield
    SourceFile(module, imports, tree) +: rest.flatten


def lift(source: SourceFile): Either[Err, List[Ir]] =
  for
    ir0 <- typeless.lift(source.tree)
    ir1  = typeless.pass(ir0)
    ir2 <- typed.lift(ir1)
  yield
    ir2


def readFile(path: String) =
  Source.fromFile(path).getLines.mkString("\n")

def loadSource(name: String) =
  readFile(internalPath(name))

def internalPath(name: String) =
  s"./lib/$name.bb"

def refFileName(ref: ast.Ref) =
  ref.id.lexeme
