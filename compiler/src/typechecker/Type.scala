package bananabread
package typechecker
package ty


sealed trait Type
case object I32 extends Type
case object Str extends Type
case object Symbol extends Type
case class Var(id: Int) extends Type
case class Lambda(in: List[Type], out: List[Type]) extends Type
