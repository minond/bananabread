package bananabread
package typechecker
package ty

import utils.Print


sealed trait Type
case object I32 extends Type with Print("I32")
case object Str extends Type with Print("Str")
case object Symbol extends Type with Print("Symbol")
case object Bool extends Type with Print("Bool")
case class Var(id: Int) extends Type
case class Lambda(in: List[Type], out: List[Type]) extends Type with Print(s"(${in.mkString(", ")}) -> (${out.mkString(", ")})")
