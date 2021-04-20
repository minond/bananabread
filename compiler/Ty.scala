package bananabread
package ty


sealed trait Type
case object I32 extends Type
case object Str extends Type
case class Var(id: Int) extends Type
case class Lambda(in: List[Type], out: List[Type]) extends Type


val freshIds = LazyList.from(1).sliding(1)

def fresh() =
    Var(freshIds.next.head)
