package bananabread
package parsing.opcode

import opcode.Instruction


sealed trait Err

case class Tree(nodes: List[Instruction])

def parse(sourceString: String): Either[Err, Tree] =
  ???
