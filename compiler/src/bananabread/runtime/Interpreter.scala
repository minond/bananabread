package bananabread
package runtime

import register.Registers
import instruction.{Code, labels, constants}
import value.Value

import scala.collection.mutable.Stack


sealed trait Dispatch
case object Stop extends Dispatch
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


class Interpreter(codes: List[Code], private val debug: Boolean = false, private val step: Boolean = false):
  val stack = Stack[Value]()
  val frames = Frames()
  val registers = Registers()

  val labels = codes.labels
  val constants = codes.constants

  def debugging = Interpreter(codes, true, step)
  def stepping  = Interpreter(codes, debug, true)

  def run =
    while registers.pc.value != -1 do
      debugBefore
      next
      debugAfter
      waitForUser

  def next =
    handle(codes(registers.pc.value), State.from(this)) match
      case Stop        => registers.pc(-1)
      case Cont        => registers.pc(registers.pc.value + 1)
      case Goto(label) => registers.pc(labels(label))
      case Jump(index) => registers.pc(index)
      case Fatal(msg)  => throw Exception(msg) /* XXX */

  def debugBefore =
    if debug then
      println(s"INSTRUCTION ${codes(registers.pc.value)}")
      println(s"\t\t\tSTACK: $stack")
      println(s"\t\t\tREGISTERS: $registers")

  def debugAfter =
    if debug then
      println(s"\t\t\tSTACK: $stack")
      println(s"\t\t\tREGISTERS: $registers")
      println(s"\t\t\tFINISH")

  def waitForUser =
    if step then
      scala.io.StdIn.readLine()
