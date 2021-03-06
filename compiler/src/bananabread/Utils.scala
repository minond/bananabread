package bananabread
package utils

import scala.collection.mutable.Map
import scala.reflect.ClassTag
import scala.util.{Try, Success, Failure}


extension [T](xs: List[T])
  def onlys[X <: T : ClassTag]: Either[List[T], List[X]] =
    xs.foldLeft[Either[List[T], List[X]]](Right(List())) {
      case (acc, x : X) => acc.flatMap(xs => Right(xs :+ x))
      case _ => return Left(xs)
    }

  def withonly[X <: T : ClassTag]: List[X] =
    xs.foldLeft[List[X]](List.empty) {
      case (acc, x: X) => acc :+ x
      case (acc, _) => acc
    }

  def without[X <: T : ClassTag] =
    xs.filter {
      case _: X => false
      case _    => true
    }


extension (strs: List[{ def toString(): String }])
  def groupedIds =
    if strs.size == 1
    then strs.head
    else s"(${strs.mkString(", ")})"


extension [L, R](eithers: List[Either[L, R]])
  def squished: Either[L, List[R]] =
    eithers.iterator.squished


extension [L, R](eithers: Iterator[Either[L, R]])
  def squished: Either[L, List[R]] =
    eithers.foldLeft[Either[L, List[R]]](Right(List())) {
      (acc, x) =>
        acc.flatMap(xs => x.map(xs :+ _))
    }


extension [T <: Any](obj: T)
  def isAn[X : ClassTag]: Boolean = isA[X]
  def isA[X : ClassTag]: Boolean = obj match
    case _ : X => true
    case _     => false

  def asList: List[T] =
    List(obj)


extension (str: String)
  def safeToInt: Either[Throwable, Int] =
    Try { str.toInt } match
      case Failure(err) => Left(err)
      case Success(int) => Right(int)

  def safeToFloat: Either[Throwable, Float] =
    Try { str.toFloat } match
      case Failure(err) => Left(err)
      case Success(flt) => Right(flt)


trait Print(inner: String = ""):
  self =>
    override def toString =
      if inner == ""
      then getClass.getSimpleName.toUpperCase
      else inner


val ids = LazyList.from(1).sliding(1)
def genUnique(str: String) =
  s"$str${ids.next.head}"

val ptrs = Map[String, String]()
def ptrOf(header: String, hash: Int) = ptrs.get(header + hash.toString) match
  case Some(ptr) => ptr
  case None =>
    val ptr = genUnique(header)
    ptrs.update(header + hash.toString, ptr)
    ptr

trait Ptr(prefix: String):
  def ptr = ptrOf(prefix, hashCode)

trait PtrWith(prefix: String, hashCodeF: () => Int):
  def ptr = ptrOf(prefix, hashCodeF())
