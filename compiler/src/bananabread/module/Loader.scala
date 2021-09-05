package bananabread
package module


import scala.io.Source


def readFile(path: String) =
  Source.fromFile(path).getLines.mkString("\n")

def loadSource(name: String) =
  readFile(internalPath(name))

def internalPath(name: String) =
  s"./lib/$name.bb"
