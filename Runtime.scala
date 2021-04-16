package sourdough
package runtime

import ir.{Typeless => tl}
import ir.Typeless.Ir
import opcode.Opcode
import value.Value

import scala.util.Random


val rand = Random.alphanumeric


case class Instruction(op: Opcode, args: Value*):
  def toList = List(this)

  override def toString = op match
    case opcode.Label => s"${args(0)}:"
    case _ => s"  $op ${args.mkString(", ")}"

def inst(op: Opcode, args: Value*) =
  Instruction(op, args:_*).toList

def lift(nodes: List[Ir]): List[Instruction] =
  nodes.flatMap(lift)

def lift(node: Ir): List[Instruction] = node match
  case _: tl.Num => push(node, ty.I32)
  case tl.App(lambda, args, _) => call(lambda, args)
  case tl.Cond(cnd, pas, fal, _) => cond(cnd, pas, fal)


def push(node: Ir, typ: ty.Type) = typ match
  case ty.I32 => inst(opcode.PushI32, value.lift(node))

def call(lambda: Ir, args: List[Ir]) =
  args.flatMap(lift) ++ inst(opcode.Call, value.lift(lambda))

def label(name: String): value.Id =
  value.Id(s"$name.${rand.take(16).mkString}")

def cond(cnd: Ir, pas: Ir, fal: Ir) =
  val lelse = label("else")
  val ldone = label("done")

  lift(cnd) ++
  inst(opcode.Jz, lelse) ++
  lift(pas) ++
  inst(opcode.Jmp, ldone) ++
  inst(opcode.Label, lelse) ++
  lift(fal) ++
  inst(opcode.Label, ldone)
