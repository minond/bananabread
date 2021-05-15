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


class Interpreter(codes: List[Code]):
  val stack = Stack[Value]()
  val frames = Frames()
  val registers = Registers()

  val labels = codes.labels
  val constants = codes.constants
