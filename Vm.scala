package sourdough
package vm

import opcode.Opcode
import runtime.Instruction
import value.Value

import scala.collection.mutable.Stack


sealed trait Pc
case object Halt extends Pc
case object Cont extends Pc
case class Jump(label: String) extends Pc


class Machine(instructions: Seq[Instruction]):
  val stack = Stack[Value]()
  var pc = 0
  var running = true

  def next: Unit =
    if !running then return

    eval(instructions(pc)) match
      case Halt => running = false
      case Cont => pc = pc + 1
      case Jump(label) =>

  def eval(instruction: Instruction): Pc = (instruction.opcode, instruction.args.toList) match
    case (opcode.Halt, _) => Halt
    case (opcode.Label, _) => Cont
    case (opcode.PushI32, (v : value.I32) :: Nil) => push(v); Cont
    case (opcode.Call, (v : value.Id) :: Nil) => call(v); Cont

  def push(v: value.Value) =
    stack.push(v)

  def call(v: value.Id) = v.label match
    case "+" =>
      (stack.pop, stack.pop) match
        case (value.I32(lhs), value.I32(rhs)) =>
          stack.push(value.I32(lhs + rhs))
