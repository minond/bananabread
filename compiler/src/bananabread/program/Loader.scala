package bananabread
package program

import parsing.ast
import ir.{typed, typeless, linked}
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


def lift2(sources: List[SourceFile]): Either[Err, Program] =
  val main = sources.head
  val importedSources = main.imports.map { stmt => findSourceFile(stmt.name, sources) }.flatten
  println("importedSources")
  println(importedSources)

  // if importedSources.isEmpty
  // then lift(main)

  val liftedImports =
    if importedSources.isEmpty
    then List.empty
    else lift2(importedSources)
  println("liftedImports")
  println(liftedImports)

  ???

def lift(source: SourceFile): Either[Err, List[Ir]] =
  for
    ir0 <- typeless.lift(source.tree)
    ir1  = typeless.pass(ir0)
    ir2 <- linked.lift(ir1, source.imports)
    ir3 <- typed.lift(ir2)
  yield
    ir3

def findSourceFile(ref: ast.Ref, sources: List[SourceFile]): Option[SourceFile] =
  sources.find {
    case SourceFile(Some(module), _, _) =>
      module.name.id.lexeme == ref.id.lexeme
    case _ =>
      false
  }

def readFile(path: String) =
  Source.fromFile(path).getLines.mkString("\n")

def loadSource(name: String) =
  readFile(internalPath(name))

def internalPath(name: String) =
  s"./lib/$name.bb"

def refFileName(ref: ast.Ref) =
  ref.id.lexeme
