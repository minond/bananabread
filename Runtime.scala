package sourdough
package runtime

import value.Value
import scala.collection.mutable.Stack


case class Instruction(opcode: Opcode, args: List[Value] = List.empty)

enum Pc:
  case Halt
  case Cont
  case Jump(label: String)

enum Opcode:
  case Halt
  case PushI32
  case Call


class Machine(instructions: Seq[Instruction]):
  val stack = Stack[Value]()
  var pc = 0
  var running = true

  def next: Unit =
    if !running then return

    eval(instructions(pc)) match
      case Pc.Halt => running = false
      case Pc.Cont => pc = pc + 1
      case Pc.Jump(label) =>

  def eval(instruction: Instruction): Pc = (instruction.opcode, instruction.args) match
    case (Opcode.Halt, _) =>
      halt
    case (Opcode.PushI32, (v : value.I32) :: Nil) =>
      push(v)
      cont
    case (Opcode.Call, (v : value.Id) :: Nil) =>
      call(v)
      cont

  def call(v: value.Id) = v.label match
    case "+" =>
      (pop, pop) match
        case (value.I32(lhs), value.I32(rhs)) =>
          push(value.I32(lhs + rhs))

  def pop = stack.pop
  def push(v: Value) = stack.push(v)

  def cont : Pc = Pc.Cont
  def halt : Pc = Pc.Halt
