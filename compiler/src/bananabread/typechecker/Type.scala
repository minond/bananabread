package bananabread
package typechecker
package ty

import utils.{Print, groupedIds}


sealed trait Type
sealed trait Applicable extends Type { def app(c: Int) : Type }

case object I32 extends Type with Print("I32")
case object Str extends Type with Print("Str")
case object Symbol extends Type with Print("Symbol")
case object Bool extends Type with Print("Bool")
case object Void extends Type with Print("Void")
case class Var(id: Int) extends Type
case class Tuple(items: List[Type]) extends Type with Print(s"(${items.mkString(", ")})")

case object Lista extends Type with Print("List"), Applicable {
  def app(c: Int): Type = I32
}

case class Lambda(in: List[Type], out: Type) extends Type with Applicable, Print(s"${in.groupedIds} -> $out") {
  def app(c: Int): Type =
    if c == in.size
    then out
    else curry(c)

  def curry(c: Int): Lambda =
    Lambda(in.drop(c), out)

  def zip(other: Lambda) =
    in.zip(other.in) :+ (out, other.out)
}
