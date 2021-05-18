package bananabread
package backend.bytecode

import backend.opcode.GeneratorError
import runtime.instruction._
import ir.typeless.Ir


type Bytes = List[Byte]


def compile(nodes: List[Ir]): Either[GeneratorError, Bytes] =
  for
    codes <- backend.opcode.compile(nodes)
    bytes  = codes.map(generate)
  yield
    bytes.flatten

def generate(code: Code): Bytes = code match
  case op: Label => generateLabel(op)

def generateLabel(op: Label): Bytes =
  ???
