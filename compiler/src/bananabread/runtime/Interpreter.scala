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


case class State(
  val stack: Stack[Value],
  val frames: Frames,
  val registers: Registers,
  val constants: Map[String, Value],
  val labels: Map[String, Int],
)

object State:
  def from(i: Interpreter) =
    State(i.stack, i.frames, i.registers, i.constants, i.labels)


class Interpreter(codes: List[Code], debug: Boolean = false):
  val stack = Stack[Value]()
  val frames = Frames()
  val registers = Registers()

  val labels = codes.labels
  val constants = codes.constants

  def debugging =
    Interpreter(codes, true)

  def run =
    while running do
      debugPrefix
      next
      debugSuffix

  def running =
    registers.pc.value != -1

  def next =
    handle(codes(registers.pc.value), State.from(this)) match
      case Halt        => registers.pc(-1)
      case Cont        => registers.pc(registers.pc.value + 1)
      case Goto(label) => registers.pc(labels(label))
      case Jump(index) => registers.pc(index)
      case Fatal(msg)  => throw Exception(msg) /* XXX */

  def debugPrefix =
    if debug then
      println(s"========= START ${codes(registers.pc.value)}")
      println(s"STACK: $stack")
      println(s"REGISTERS: $registers")

  def debugSuffix =
    if debug then
      println(s"STACK: $stack")
      println(s"REGISTERS: $registers")
      println(s"========= FINISH ${codes(registers.pc.value)}")


def handle(code: Code, state: State): Dispatch = code match
  case _: instruction.Value => Cont
  case _: instruction.Label => Cont
  case instruction.Halt  => Halt
  case op: Jz    => handleJz(op, state)
  case op: Jmp   => handleJmp(op, state)
  case op: Push  => handlePush(op, state)
  case op: Call  => handleCall(op, state)
  case Call0     => handleCall0(state)
  case Ret       => handleRet(state)
  case Swap      => handleSwap(state)
  case op: Mov   => handleMov(op, state)
  case op: Load  => handleLoad(op, state)
  case op: Store => handleStore(op, state)
  case Println   => handlePrintln(state)
  case Concat    => handleConcat(state)
  case op: Add   => handleAdd(op, state)
  case op: Sub   => handleSub(op, state)

def handleJz(op: Jz, state: State): Dispatch =
  ???

def handleJmp(op: Jmp, state: State): Dispatch =
  ???

def handlePush(op: Push, state: State): Dispatch = op match
  case Push(I32, v: value.I32) =>
    state.stack.push(v)
    Cont
  case Push(Ptr, v: value.Id) =>
    state.stack.push(v)
    Cont
  case Push(Scope, value.Id(label)) =>
    val scope = value.Scope(label, state.frames.curr)
    state.stack.push(scope)
    Cont
  case Push(Const, value.Id(label)) =>
    state.constants.get(label) match
      case Some(v) =>
        state.stack.push(v)
        Cont
      case None =>
        Fatal(s"missing const: $label")
  case _ =>
    Fatal("bad push")

def handleCall(op: Call, state: State): Dispatch = state.frames.curr.get(op.label) match
  case None =>
    state.frames.next
    Goto(op.label)
  case Some(ptr: value.Id) =>
    state.frames.next
    Goto(ptr.label)
  case Some(value.Scope(label, frame)) =>
    state.frames.from(frame)
    Goto(label)
  case Some(_) =>
    Fatal("bad call")

def handleCall0(state: State): Dispatch =
  ???

def handleRet(state: State): Dispatch = state.stack.pop match
  case value.I32(addr) =>
    state.frames.prev
    Jump(addr)
  case _ =>
    Fatal("bad ret: missing return address")

def handleSwap(state: State): Dispatch =
  val a = state.stack.pop
  val b = state.stack.pop
  state.stack.push(a)
  state.stack.push(b)
  Cont

def handleMov(op: Mov, state: State): Dispatch = op match
  case Mov(reg, Some(offset)) =>
    val curr = state.registers.get(reg)
    val next = value.I32(curr.value + offset.value)
    state.registers.set(reg, next)
    Cont
  case Mov(reg, None) => state.stack.pop match
    case addr: value.I32 =>
      state.registers.set(reg, addr)
      Cont
    case value.Id(label) =>
      state.labels.get(label) match
        case Some(i) =>
          state.registers.set(reg, i)
          Cont
        case None =>
          Fatal(s"bad mov: missing label $label")
    case value.Scope(label, frame) =>
      state.labels.get(label) match
        case Some(i) =>
          state.frames.from(frame)
          state.registers.set(reg, value.I32(i))
          Cont
        case None =>
          Fatal(s"bad mov: missing scope $label")
    case _ =>
      Fatal("bad mov: invalid stack entry")

def handleLoad(op: Load, state: State): Dispatch = state.frames.curr.get(op.label) match
  case None =>
    println(s"loading ${op}")
    val value = state.constants(op.label)
    state.stack.push(value)
    Cont
  case Some(v) =>
    state.stack.push(v)
    Cont

def handleStore(op: Store, state: State): Dispatch =
  state.frames.curr.put(op.label, state.stack.pop)
  Cont

def handlePrintln(state: State): Dispatch =
  println(state.stack.head)
  Cont

def handleConcat(state: State): Dispatch =
  ???

def handleAdd(op: Add, state: State): Dispatch =
  ???

def handleSub(op: Sub, state: State): Dispatch =
  ???
