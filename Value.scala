package sourdough
package value

import ir.Typeless


sealed trait Value
case class I32(value: Integer) extends Value
case class Str(value: String) extends Value
case class Id(label: String) extends Value


def lift(nodes: List[Typeless.Ir]): List[Value] =
  nodes.map(lift)

def lift(node: Typeless.Ir): Value = node match
  case Typeless.Num(ast.Num(value, _)) => I32(value.toInt)
  case Typeless.Str(ast.Str(value, _)) => Str(value)
  case Typeless.Id(ast.Id(label, _)) => Id(label)
