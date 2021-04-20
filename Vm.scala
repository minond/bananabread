package sourdough
package vm

import opcode.Opcode
import runtime.Instruction
import value.Value

import scala.collection.mutable.{Stack, Map}


type Frame = Map[String, Value]
type Registers = Map[value.Id, value.I32]


sealed trait Pc
case object Halt extends Pc
case object Cont extends Pc
case class Goto(label: String) extends Pc
case class Jump(pc: Int) extends Pc


object Reg:
  val Pc = value.Id("%pc")
  val Rpc = value.Id("%rpc")
  val Jmp = value.Id("%jmp")


class Machine(instructions: Seq[Instruction]):
  val stack = Stack[Value]()
  val frame = Stack[Frame](Map.empty)

  val registers: Registers = Map(
    Reg.Pc -> value.I32(0),
    Reg.Rpc -> value.I32(0),
    Reg.Jmp -> value.I32(0),
  )

  val labels = instructions.zipWithIndex.collect {
    case (Instruction(opcode.Label, value.Id(label)), index) => (label, index)
  }.toMap

  def pc = registers.get(Reg.Pc).get
  def rpc = registers.get(Reg.Rpc).get
  def jmp = registers.get(Reg.Jmp).get

  def running = pc.value != -1

  def next: Unit =
    if !running then return

    eval(instructions(pc.value)) match
      case Halt => registers.update(Reg.Pc, value.I32(-1))
      case Cont => registers.update(Reg.Pc, value.I32(pc.value + 1))
      case Goto(label) => registers.update(Reg.Pc, value.I32(labels(label)))
      case Jump(next) => registers.update(Reg.Pc, value.I32(next))

  def eval(instruction: Instruction): Pc = (instruction.op, instruction.args.toList) match
    case (opcode.Halt, _) =>
      Halt
    case (opcode.Label, _) =>
      Cont
    case (opcode.Jz, value.Id(label) :: Nil) =>
      stack.pop match
        case value.I32(0) => Goto(label)
        case _ => Cont
    case (opcode.Jz, _) =>
      /* bad call */
      ???
    case (opcode.Jmp, value.Id(label) :: Nil) =>
      Goto(label)
    case (opcode.Jmp, _) =>
      /* bad call */
      ???
    case (opcode.PushI32, (v : value.I32) :: Nil) =>
      stack.push(v)
      Cont
    case (opcode.PushI32, _) =>
      /* missing impl */
      ???
    case (opcode.PushReg, reg :: Nil) =>
      reg match
        case Reg.Pc =>
          stack.push(pc)
          Cont
        case Reg.Rpc =>
          stack.push(registers.get(Reg.Rpc).get)
          Cont
        case Reg.Jmp =>
          stack.push(registers.get(Reg.Jmp).get)
          Cont
        case _ =>
          /* bad reg */
          ???
    case (opcode.PushReg, _) =>
      /* bad call */
      ???
    case (opcode.PushPtr, ptr :: Nil) =>
      stack.push(ptr)
      Cont
    case (opcode.PushPtr, _) =>
      /* bad call */
      ???
    case (opcode.Run, (v : value.Id) :: Nil) =>
      run(v)
      Cont
    case (opcode.Run, _) =>
      /* missing impl */
      ???
    case (opcode.Call, value.Id(label) :: Nil) =>
      frame.head.get(label) match
        case None =>
          frame.push(Map.empty)
          Goto(label)
        case Some(ptr: value.Id) =>
          frame.push(Map.empty)
          Goto(ptr.label)
        case Some(_) =>
          /* bad call */
          ???
    case (opcode.Call, Nil) =>
      frame.push(Map.empty)
      Jump(jmp.value)
    case (opcode.Call, _) =>
      /* bad call */
      ???
    case (opcode.Ret, Nil) =>
      frame.pop
      Jump(rpc.value + 2)
    case (opcode.Ret, _) =>
      /* missing impl */
      ???
    case (opcode.Mov, (dest: value.Id) :: (source: value.Id) :: Nil) =>
      movregreg(dest, source)
      Cont
    case (opcode.Mov, (dest: value.Id) :: Nil) =>
      movstackreg(dest)
      Cont
    case (opcode.Mov, _) =>
      /* missing impl */
      ???
    case (opcode.StoreI32, value.Id(label) :: Nil) =>
      frame.head.put(label, stack.pop)
      Cont
    case (opcode.StoreI32, _) =>
      /* bad call */
      ???
    case (opcode.StorePtr, value.Id(label) :: Nil) =>
      frame.head.put(label, stack.pop)
      Cont
    case (opcode.StorePtr, _) =>
      /* bad call */
      ???
    case (opcode.LoadI32, value.Id(label) :: Nil) =>
      frame.head.get(label) match
        case None =>
          /* undeclared var */
          ???
        case Some(v) =>
          stack.push(v)
          Cont
    case (opcode.LoadI32, _) =>
      /* bad call */
      ???

  def movregreg(dest: value.Id, source: value.Id) = (dest, source, registers.get(source)) match
    case (Reg.Rpc, Reg.Pc, Some(value)) =>
      registers.update(Reg.Rpc, value)
    case _ =>
      /* not implemented  */
      ???

  def movstackreg(dest: value.Id) = stack.pop match
    case addr: value.I32 =>
      registers.update(dest, addr)
    case value.Id(label) =>
      registers.update(dest, value.I32(labels(label)))
    case _ =>
      /* not implemented  */
      ???

  def run(v: value.Id) = v.label match
    case "+" =>
      (stack.pop, stack.pop) match
        case (value.I32(lhs), value.I32(rhs)) =>
          stack.push(value.I32(lhs + rhs))
        case _ =>
          /* bad call */
          ???
