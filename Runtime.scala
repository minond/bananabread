package sourdough
package runtime

import ir.{Typeless => tl}
import ir.Typeless.Ir
import opcode.Opcode
import value.Value


case class Instruction(opcode: Opcode, args: Value*):
  def toList = List(this)


def lift(nodes: List[Ir]): List[Instruction] =
  nodes.flatMap(lift)

def lift(node: Ir): List[Instruction] = node match
  case _: tl.Num => push(node, ty.I32)
  case tl.App(lambda, args, _) => call(lambda, args)


def push(node: Ir, typ: ty.Type) = typ match
  case ty.I32 => Instruction(opcode.PushI32, value.lift(node)).toList

def call(lambda: Ir, args: List[Ir]) =
  args.flatMap(lift) :+ Instruction(opcode.Call, value.lift(lambda))
