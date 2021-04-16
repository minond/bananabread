package sourdough
package value

import ir.Typeless => tl
import ir.Typeless.Ir
import utils.Print


sealed trait Value
case class I32(value: Integer) extends Value with Print(s"#$value")
case class Str(value: String) extends Value with Print(s""""$value"""")
case class Id(label: String) extends Value with Print(s"$$$label")


def lift(nodes: List[Ir]): List[Value] =
  nodes.map(lift)

def lift(node: Ir): Value = node match
  case tl.Num(ast.Num(value, _)) => I32(value.toInt)
  case tl.Str(ast.Str(value, _)) => Str(value)
  case tl.Id(ast.Id(label, _)) => Id(label)
