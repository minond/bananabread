package bananabread
package backend.bytecode

import backend.opcode.error.GeneratorErr
import runtime.instruction._
import ir.typeless.Ir


type Bytes = List[Byte]


def compile(nodes: List[Ir]): Either[GeneratorErr, Bytes] =
  for
    codes <- backend.opcode.compile(nodes)
    bytes  = codes.map(generate)
  yield
    bytes.flatten

def generate(code: Code): Bytes = code match
  case op: Label => generateLabel(op)
  case _ => ???

def generateLabel(op: Label): Bytes =
  ???
