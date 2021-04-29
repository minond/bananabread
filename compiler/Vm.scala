package bananabread
package vm

import opcode.{Opcode, Instruction, Exposed}
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
  val Lr = value.Id("%lr")
  val Jmp = value.Id("%jmp")


class Machine(instructions: Seq[Instruction], info: Boolean = false, prompt: Boolean = false):
  val stack = Stack[Value]()
  val frame = Stack[Frame](Map.empty)

  val registers: Registers = Map(
    Reg.Pc -> value.I32(0),
    Reg.Lr -> value.I32(0),
    Reg.Jmp -> value.I32(0),
  )

  val labels = instructions.zipWithIndex.collect {
    case (Instruction(opcode.Label, value.Id(label)), index) => (label, index)
  }.toMap

  def pc = registers.get(Reg.Pc).get
  def lr = registers.get(Reg.Lr).get
  def jmp = registers.get(Reg.Jmp).get

  def running = pc.value != -1

  def next: Unit =
    if !running then return

    if info then
      println(s"====================================")
      printInfo
      println(s"INS: ${instructions(pc.value)}")

    if prompt then
      scala.io.StdIn.readLine()

    eval(instructions(pc.value)) match
      case Halt => registers.update(Reg.Pc, value.I32(-1))
      case Cont => registers.update(Reg.Pc, value.I32(pc.value + 1))
      case Goto(label) => registers.update(Reg.Pc, value.I32(labels(label)))
      case Jump(next) => registers.update(Reg.Pc, value.I32(next))

    if info then
      printInfo
      println(s"====================================")

  def printInstructions =
    instructions.zipWithIndex
      .foreach { (ins, i) => println(s"$i\t$ins") }

  def printInfo =
    println(s"STK: $stack")
    println(s"REG: $registers")
    println(s"FRM: $frame")

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
    case (opcode.Push(opcode.I32), (v : value.I32) :: Nil) =>
      stack.push(v)
      Cont
    case (opcode.Push(_), _) =>
      /* missing impl */
      ???
    case (opcode.PushReg, reg :: value.I32(offset) :: Nil) =>
      reg match
        case Reg.Pc =>
          stack.push(value.I32(pc.value + offset))
          Cont
        case Reg.Lr =>
          stack.push(value.I32(lr.value + offset))
          Cont
        case Reg.Jmp =>
          stack.push(value.I32(jmp.value + offset))
          Cont
        case _ =>
          /* bad reg */
          ???
    case (opcode.PushReg, reg :: Nil) =>
      reg match
        case Reg.Pc =>
          stack.push(pc)
          Cont
        case Reg.Lr =>
          stack.push(lr)
          Cont
        case Reg.Jmp =>
          stack.push(jmp)
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
    case (handled: opcode.Run, Nil) =>
      handled.handler(this)
      Cont
    case (handled: opcode.Run, _) =>
      /* bad call */
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
    case (opcode.Swap, Nil) =>
      val a = stack.pop
      val b = stack.pop
      stack.push(a)
      stack.push(b)
      Cont
    case (opcode.Swap, _) =>
      /* bad call */
      ???
    case (opcode.Ret, Nil) =>
      stack.pop match
        case value.I32(addr) =>
          frame.pop
          Jump(addr)
        case _ =>
          /* bad ret addr */
          ???
    case (opcode.Ret, _) =>
      /* missing impl */
      ???
    case (opcode.Mov, (dest: value.Id) :: Nil) =>
      movstackreg(dest)
      Cont
    case (opcode.Mov, _) =>
      /* missing impl */
      ???
    case (opcode.Store(opcode.I32), value.Id(label) :: Nil) =>
      frame.head.put(label, stack.pop)
      Cont
    case (opcode.Store(_), _) =>
      /* bad call */
      ???
    case (opcode.StorePtr, value.Id(label) :: Nil) =>
      frame.head.put(label, stack.pop)
      Cont
    case (opcode.StorePtr, _) =>
      /* bad call */
      ???
    case (opcode.Load(opcode.I32), value.Id(label) :: Nil) =>
      frame.head.get(label) match
        case None =>
          /* undeclared var */
          ???
        case Some(v) =>
          stack.push(v)
          Cont
    case (opcode.Load(_), _) =>
      /* bad call */
      ???

  def movstackreg(dest: value.Id) = stack.pop match
    case addr: value.I32 =>
      registers.update(dest, addr)
    case value.Id(label) =>
      registers.update(dest, value.I32(labels(label)))
    case _ =>
      /* not implemented  */
      ???

  def run(v: value.Id) =
    Exposed.lookup(v.label).handler(this)

  def bini32op(f: (Integer, Integer) => Integer) =
    (stack.pop, stack.pop) match
      case (value.I32(rhs), value.I32(lhs)) =>
        stack.push(value.I32(f(lhs, rhs)))
      case _ =>
        /* bad call */
        ???
