package bananabread
package module


import scala.io.Source


def loadSource(name: String) =
  Source.fromFile(internalPath(name))
        .getLines
        .mkString("\n")

def internalPath(name: String) =
  s"./lib/$name.bb"
