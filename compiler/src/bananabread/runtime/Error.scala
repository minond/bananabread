package bananabread
package runtime.error

import runtime.instruction.{Code, Instruction}
import runtime.register.Registers


sealed trait RuntimeErr
case class FatalErr(msg: String, instruction: Instruction, codes: List[Code], registers: Registers) extends RuntimeErr
