package bananabread
package runtime
package error

import instruction.{Code, Instruction}
import register.Registers


case class RuntimeErr(msg: String, instruction: Instruction, codes: List[Code], registers: Registers)
