package bananabread
package typechecker
package error

import ty._
import ir.linked.Ir
import parsing.ast


sealed trait InferenceErr
case class LookupErr(label: String, ir: Ir) extends InferenceErr
case class TypeMismatchErr(expected: Type, got: Type, node: Ir) extends InferenceErr
case class GenTypeMismatchErr[T](expected: Class[T], got: Type, node: Ir) extends InferenceErr
case class UnknowTypeErr(tag: ast.TyId) extends InferenceErr
case class UnunifiedTypeVarErr(v: Var, node: Ir) extends InferenceErr
case class UnificationErr(expected: Type, got: Type, node: Ir) extends InferenceErr
case class ArgUnificationErr(expected: Type, got: Type, node: Ir, index: Int) extends InferenceErr
