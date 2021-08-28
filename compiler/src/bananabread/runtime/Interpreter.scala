package bananabread
package runtime

import error._
import register.{Registers, Sp}
import memory.Stack
import instruction.{Code, Instruction, labels, constants, pp}
import value.Value


sealed trait Dispatch
case object Stop extends Dispatch
case object Cont extends Dispatch
case class Goto(label: String) extends Dispatch
case class Jump(index: Int) extends Dispatch
case class Error(msg: String, instruction: Instruction) extends Dispatch


case class State(
  val stack: Stack,
  val frames: Frames,
  val registers: Registers,
  val constants: Map[String, Value],
  val labels: Map[String, Int],
):
  def pop: Value =
    registers.dec(Sp)
    stack.get(registers.sp)

  def push(v: Value) =
    stack.set(registers.sp, v)
    registers.inc(Sp)

object State:
  def from(i: Interpreter) =
    State(i.stack, i.frames, i.registers, i.constants, i.labels)


class Interpreter(codes: List[Code], private val debug: Boolean = false, private val step: Boolean = false):
  val stack = Stack()
  val frames = Frames()
  val registers = Registers()

  val labels = codes.labels
  val constants = codes.constants

  def debugging = Interpreter(codes, true, step)
  def stepping  = Interpreter(codes, debug, true)

  def run: Either[RuntimeErr, State] =
    showState
    while registers.pc.value != -1 do
      showInstruction
      next match
        case Some(Error(msg, ins)) => return Left(RuntimeErr(msg, ins, codes, registers))
        case _ =>
      showState
      waitForUser
    Right(State.from(this))

  def next: Option[Error] =
    handle(codes(registers.pc.value), State.from(this)) match
      case Stop        => registers.pc(-1); None
      case Cont        => registers.pc(registers.pc.value + 1); None
      case Goto(label) => registers.pc(labels(label)); None
      case Jump(index) => registers.pc(index); None
      case err: Error  => Some(err)

  def showInstruction =
    if debug then
      println(s"- Instruction --- ${pp(codes(registers.pc.value), false)}")

  def showState =
    if debug then
      println(s"  Stack --------- [${stack}]")
      println(s"  Registers ----- {$registers}")
      println(s"  Frame --------- {${frames.curr}}")

  def waitForUser =
    if step && scala.io.StdIn.readLine() == "quit" then
      throw new Exception("quit")
