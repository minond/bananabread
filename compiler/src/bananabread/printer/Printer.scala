package bananabread
package printer

import runtime.instruction.Code
import error.Err


def pp(codes: List[Code]): String                   = instructions.pp(codes)
def pp(code: Code, align: Boolean = false): String  = instructions.pp(code, align)
def pp(err: Err, source: String): String            = errors.pp(err, source)
