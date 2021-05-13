package bananabread
package runtime.value

import parsing.ast
import runtime.Frame

import ir.typeless
import ir.typeless.Ir
import show.Print


sealed trait Value
case class I32(value: Int) extends Value with Print(value.toString)
case class Str(value: String) extends Value with Print(value)
case class Id(label: String) extends Value with Print(label)
case class Scope(label: String, frame: Frame) extends Value with Print(s"<$label>")
case class Symbol(value: String) extends Value with Print(value)


def lift(nodes: List[Ir]): List[Value] =
  nodes.map(lift)
def lift(node: Ir): Value = node match
  case typeless.Num(ast.Num(value, _)) => I32(value.toInt)
  case typeless.Str(ast.Str(value, _)) => Str(value)
  case typeless.Id(ast.Id(label, _)) => Id(label)
  case typeless.Symbol(ast.Symbol(value, _)) => Symbol(value)
  case _: typeless.App => ???
  case _: typeless.Lambda => ???
  case _: typeless.Cond => ???
  case _: typeless.Let => ???
  case _: typeless.Begin => ???
  case typeless.Def(_, value, _) => lift(value)
