package sourdough
package runtime

import value.Value
import scala.collection.mutable.Stack


case class Instruction(opcode: Opcode, args: List[Value] = List.empty)

enum Opcode:
  case Halt
  case PushI32
  case Call
