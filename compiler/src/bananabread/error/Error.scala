package bananabread
package error

import backend.bytecode.error => genop
import parsing.error => parse
import runtime.error => runtime
import typechecker.error => typechecker
import loader.error => loader
import ir.typeless.error => typelessIr
import ir.linked.error => linkedIr
import ir.typed.error => typedIr
import ir.stitched.error => stitchedIr

type Err = parse.SyntaxErr
         | genop.GeneratorErr
         | runtime.RuntimeErr
         | typechecker.InferenceErr
         | loader.LoadErr
         | typelessIr.LiftErr
         | linkedIr.LiftErr
         | typedIr.LiftErr
         | stitchedIr.LiftErr
