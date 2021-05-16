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
      case Stop        => registers.pc(-1)
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
      println(s"========= FINISH")
