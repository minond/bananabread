package bananabread
package runtime.value

import parsing.ast

import ir.Typeless => tl
import ir.Typeless.Ir
import utils.Print


sealed trait Value
case class I32(value: Int) extends Value with Print(value.toString)
case class Str(value: String) extends Value with Print(value)
case class Id(label: String) extends Value with Print(label)
case class Scope(label: String, frame: runtime.vm.Frame) extends Value with Print(s"<$label>")
case class Symbol(value: String) extends Value with Print(value)


def lift(nodes: List[Ir]): List[Value] =
  nodes.map(lift)

def lift(node: Ir): Value = node match
  case tl.Num(ast.Num(value, _)) => I32(value.toInt)
  case tl.Str(ast.Str(value, _)) => Str(value)
  case tl.Id(ast.Id(label, _)) => Id(label)
  case tl.Symbol(ast.Symbol(value, _)) => Symbol(value)
  case _: tl.App => ???
  case _: tl.Lambda => ???
  case _: tl.Cond => ???
  case _: tl.Let => ???
  case _: tl.Begin => ???
  case tl.Def(_, value, _) => lift(value)