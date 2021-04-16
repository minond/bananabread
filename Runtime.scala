package sourdough
package runtime

import ir.Typeless
import opcode.Opcode
import value.Value


case class Instruction(opcode: Opcode, args: Value*)
