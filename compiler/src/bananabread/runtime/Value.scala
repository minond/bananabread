package bananabread
package runtime
package value

import parsing.ast
import ir.typeless
import ir.typeless.Ir
import utils.Print


sealed trait Value
case class I32(value: Int) extends Value with Print(value.toString)
case class Str(value: String) extends Value with Print(value)
case class Id(label: String) extends Value with Print(label)
case class Scope(label: String, frame: Frame) extends Value with Print(s"<$label>")
case class Symbol(value: String) extends Value with Print(value)

sealed trait Bool extends Value
case object True extends Bool with Print("true")
case object False extends Bool with Print("false")

sealed trait Pointer extends Value
case class Ptr(addr: Int) extends Pointer with Print(s"<$addr>")
case object Nullptr extends Pointer with Print("nullptr")


def lift(nodes: List[Ir]): List[Value] =
  nodes.map(lift)
def lift(node: Ir): Value = node match
  case typeless.Num(ast.Num(value, _))       => I32(value.toInt)
  case typeless.Str(ast.Str(value, _))       => Str(value)
  case _: typeless.True                      => True
  case _: typeless.False                     => False
  case typeless.Id(ast.Id(label, _))         => Id(label)
  case typeless.Symbol(ast.Symbol(value, _)) => Symbol(value)
  case typeless.Def(_, value, _)             => lift(value)
  case _: typeless.App                       => ???
  case _: typeless.Lambda                    => ???
  case _: typeless.Cond                      => ???
  case _: typeless.Let                       => ???
  case _: typeless.Begin                     => ???
  case _: typeless.Opcode                    => /* should never be reached */ ???
