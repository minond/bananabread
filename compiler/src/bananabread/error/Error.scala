package bananabread
package error

import backend.opcode.error => genop
import parsing.error => parse
import runtime.error => runtime
import typechecker.error => typechecker
import ir.typeless.error => typelessIr
import ir.typed.error => typedIr

type Err = parse.SyntaxErr
         | genop.GeneratorErr
         | runtime.RuntimeErr
         | typechecker.InferenceErr
         | typelessIr.LiftErr
         | typedIr.LiftErr
