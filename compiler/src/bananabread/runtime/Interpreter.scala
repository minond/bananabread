package bananabread
package runtime

import register._
import instruction._
import value.Value

import scala.collection.mutable.Stack


sealed trait Dispatch
case object Halt extends Dispatch
case object Cont extends Dispatch
case class Goto(label: String) extends Dispatch
case class Jump(index: Int) extends Dispatch
case class Fatal(msg: String) extends Dispatch


case class Machine(
  val stack: Stack[Value],
  val frames: Frames,
  val registers: Registers,
  val constants: Map[String, Value],
)


class Interpreter(codes: List[Code]):
  val stack = Stack[Value]()
  val frames = Frames()
  val registers = Registers()

  val labels = codes.labels
  val constants = codes.constants

  def running =
    registers.pc.value != -1

  def machine =
    Machine(stack, frames, registers, constants)

  def next =
    handle(codes(registers.pc.value), machine) match
      case Halt        => registers.pc(-1)
      case Cont        => registers.pc(registers.pc.value + 1)
      case Goto(label) => registers.pc(labels(label))
      case Jump(index) => registers.pc(index)
      case Fatal(msg)  => throw Exception(msg) /* XXX */


def handle(code: Code, machine: Machine): Dispatch = code match
  case _: instruction.Value => Cont
  case _: instruction.Label => Cont
  case instruction.Halt  => Halt
  case op: Jz    => handleJz(op, machine)
  case op: Jmp   => handleJmp(op, machine)
  case op: Push  => handlePush(op, machine)
  case op: Call  => handleCall(op, machine)
  case Call0     => handleCall0(machine)
  case Ret       => handleRet(machine)
  case Swap      => handleSwap(machine)
  case op: Mov   => handleMov(op, machine)
  case op: Load  => handleLoad(op, machine)
  case op: Store => handleStore(op, machine)
  case Println   => handlePrintln(machine)
  case Concat    => handlePrintln(machine)
  case op: Add   => handleAdd(op, machine)
  case op: Sub   => handleSub(op, machine)

def handleJz(op: Jz, machine: Machine): Dispatch =
  ???

def handleJmp(op: Jmp, machine: Machine): Dispatch =
  ???

def handlePush(op: Push, machine: Machine): Dispatch = op match
  case Push(I32, v: value.I32) =>
    machine.stack.push(v)
    Cont
  case Push(Ptr, v: value.Id) =>
    machine.stack.push(v)
    Cont
  case Push(Scope, value.Id(label)) =>
    val scope = value.Scope(label, machine.frames.curr)
    machine.stack.push(scope)
    Cont
  case Push(Const, value.Id(label)) =>
    machine.constants.get(label) match
      case None =>
        Fatal(s"missing const: $label")
      case Some(v) =>
        machine.stack.push(v)
        Cont
  case _ =>
    Fatal("bad push")

def handleCall(op: Call, machine: Machine): Dispatch = machine.frames.curr.get(op.label) match
  case None =>
    machine.frames.next
    Goto(op.label)
  case Some(ptr: value.Id) =>
    machine.frames.next
    Goto(ptr.label)
  case Some(value.Scope(label, frame)) =>
    machine.frames.from(frame)
    Goto(label)
  case Some(_) =>
    Fatal("bad call")

def handleCall0(machine: Machine): Dispatch =
  ???

def handleRet(machine: Machine): Dispatch =
  ???

def handleSwap(machine: Machine): Dispatch =
  ???

def handleMov(op: Mov, machine: Machine): Dispatch = op match
  case Mov(reg, Some(offset)) =>
    val curr = machine.registers.get(reg)
    val next = value.I32(curr.value + offset.value)
    machine.registers.set(reg, next)
    Cont
  case Mov(reg, None) =>
    val curr = machine.registers.get(reg)
    machine.stack.push(curr)
    Cont

def handleLoad(op: Load, machine: Machine): Dispatch =
  ???

def handleStore(op: Store, machine: Machine): Dispatch =
  ???

def handlePrintln(machine: Machine): Dispatch =
  ???

def handleConcat(machine: Machine): Dispatch =
  ???

def handleAdd(op: Add, machine: Machine): Dispatch =
  ???

def handleSub(op: Sub, machine: Machine): Dispatch =
  ???
