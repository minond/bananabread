package bananabread
package typechecker
package error

import ty._
import ir.typeless.Ir


sealed trait InferenceErr
case class LookupErr(label: String, ir: Ir) extends InferenceErr
case class TypeMismatchErr(expected: Type, got: Type, node: Ir) extends InferenceErr
