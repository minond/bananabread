package bananabread
package typechecker
package error

import ir.typeless.Ir


sealed trait InferenceErr
case class LookupErr(label: String, ir: Ir) extends InferenceErr
