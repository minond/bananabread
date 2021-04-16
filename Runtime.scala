package sourdough
package runtime

import opcode.Opcode
import value.Value


case class Instruction(opcode: Opcode, args: List[Value] = List.empty)
