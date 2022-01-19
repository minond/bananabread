package bananabread
package runtime
package treewalker
package error

import ir.stitched.Ir


sealed trait InterpretationError
case class LookupErr(label: String, node: Ir) extends InterpretationError
