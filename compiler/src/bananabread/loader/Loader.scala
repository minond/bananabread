package bananabread
package loader

import error._
import parsing.ast
import program.{SourceFile, sourceStructure}
import utils.squished

import scala.io.Source


type Err = LoadErr
         | parsing.error.SyntaxErr


def load(ref: ast.Ref): Either[Err, List[SourceFile]] =
  load(refFileName(ref))
def load(stmt: ast.Import): Either[Err, List[SourceFile]] =
  load(stmt.name)
def load(fileName: String): Either[Err, List[SourceFile]] =
  load(fileName, loadSource(fileName))
def load(fileName: String, code: String): Either[Err, List[SourceFile]] =
  for
    nodes <- parsing.language.parse(fileName, code)
    (module, imports, tree) = sourceStructure(nodes)
    rest <- imports.map(load).squished
  yield
    SourceFile(module, imports, tree) +: rest.flatten


def findSourceFile(ref: ast.Ref, sources: List[SourceFile]): Option[SourceFile] =
  sources.find {
    case SourceFile(Some(module), _, _) =>
      module.name.id.lexeme == ref.id.lexeme
    case _ =>
      false
  }

def refFileName(ref: ast.Ref) =
  ref.id.lexeme

def loadSource(name: String) =
  readFile(internalPath(name))

def internalPath(name: String) =
  s"./lib/$name.bb"

def readFile(path: String) =
  Source.fromFile(path).getLines.mkString("\n")
