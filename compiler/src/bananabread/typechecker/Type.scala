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
case class Tuple(items: List[Type]) extends Type with Print(s"(${items.mkString(", ")})")
case class Lambda(in: List[Type], out: List[Type]) extends Type {
  def app(c: Int): Type =
    if c == in.size
    then flatOut
    else curry(c)

  def flatOut: Type =
    if out.size == 1
    then out.head
    else Tuple(out)

  def curry(c: Int): Lambda =
    Lambda(in.drop(c), out)

  override def toString =
    if out.size == 1
    then s"(${in.mkString(", ")}) -> ${out.mkString(", ")}"
    else s"(${in.mkString(", ")}) -> (${out.mkString(", ")})"
}
