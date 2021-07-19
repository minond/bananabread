package bananabread
package runtime

import error._
import register.Registers
import instruction.{Code, Instruction, labels, constants}
import value.Value

import scala.collection.mutable.Stack


sealed trait Dispatch
case object Stop extends Dispatch
case object Cont extends Dispatch
case class Goto(label: String) extends Dispatch
case class Jump(index: Int) extends Dispatch
case class Fatal(msg: String) extends Dispatch
case class Fatal2(msg: String, instruction: Instruction) extends Dispatch


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


class Interpreter(codes: List[Code], private val debug: Boolean = false, private val step: Boolean = false):
  val stack = Stack[Value]()
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
        case Some(Fatal2(msg, ins)) => return Left(FatalErr(msg, ins, codes, registers))
        case _ =>
      showState
      waitForUser
    Right(State.from(this))

  def next: Option[Fatal2] =
    handle(codes(registers.pc.value), State.from(this)) match
      case Stop        => registers.pc(-1); None
      case Cont        => registers.pc(registers.pc.value + 1); None
      case Goto(label) => registers.pc(labels(label)); None
      case Jump(index) => registers.pc(index); None
      case Fatal(msg)  => throw Exception(msg) /* XXX */
      case err: Fatal2 => Some(err)

  def showInstruction =
    if debug then
      println(s"- Instruction --- ${codes(registers.pc.value)}")

  def showState =
    if debug then
      println(s"  Stack --------- [${stack.mkString(", ")}]")
      println(s"  Registers ----- {$registers}")

  def waitForUser =
    if step then
      scala.io.StdIn.readLine()
