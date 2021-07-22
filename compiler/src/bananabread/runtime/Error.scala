package bananabread
package runtime.error

import runtime.instruction.{Code, Instruction}
import runtime.register.Registers


case class RuntimeErr(msg: String, instruction: Instruction, codes: List[Code], registers: Registers)
