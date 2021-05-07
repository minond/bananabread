package bananabread
package runtime.value

import parsing.ast

import ir.Typeless => tl
import ir.Typeless.Ir
import utils.Print


sealed trait Value
case class I32(value: Integer) extends Value with Print(value.toString)
case class Str(value: String) extends Value with Print(value)
case class Id(label: String) extends Value with Print(label)


def lift(nodes: List[Ir]): List[Value] =
  nodes.map(lift)

def lift(node: Ir): Value = node match
  case tl.Num(ast.Num(value, _)) => I32(value.toInt)
  case tl.Str(ast.Str(value, _)) => Str(value)
  case tl.Id(ast.Id(label, _)) => Id(label)
  case _: tl.App => ???
  case _: tl.Lambda => ???
  case _: tl.Cond => ???
  case _: tl.Let => ???
  case _: tl.Begin => ???
  case tl.Def(_, value, _) => lift(value)
