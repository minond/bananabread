package sourdough
package value

import ir.{Typeless => tl}
import ir.Typeless.Ir


sealed trait Value
case class I32(value: Integer) extends Value
case class Str(value: String) extends Value
case class Id(label: String) extends Value


def lift(nodes: List[Ir]): List[Value] =
  nodes.map(lift)

def lift(node: Ir): Value = node match
  case tl.Num(ast.Num(value, _)) => I32(value.toInt)
  case tl.Str(ast.Str(value, _)) => Str(value)
  case tl.Id(ast.Id(label, _)) => Id(label)
