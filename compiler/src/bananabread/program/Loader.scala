package bananabread
package program

import ir.{typed, typeless}
import ir.typed.{Ir, Lifted}
import error.Err

import scala.io.Source


def load(fileName: String): Either[Err, List[Ir]] =
  load(fileName, loadSource(fileName))
def load(fileName: String, code: String): Either[Err, List[Ir]] =
  for
    nodes <- parsing.language.parse(fileName, code)
    (_, _, tree) = parsing.program.structure(nodes)
    ir0 <- typeless.lift(tree)
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
