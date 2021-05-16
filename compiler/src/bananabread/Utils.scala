package bananabread
package utils

import scala.reflect.ClassTag
import scala.util.{Try, Success, Failure}


extension [T](xs: List[T])
  def onlys[X : ClassTag]: Either[List[T], List[X]] =
    xs.foldLeft[Either[List[T], List[X]]](Right(List())) {
      case (acc, x : X) => acc.flatMap(xs => Right(xs :+ x))
      case _ => return Left(xs)
    }

  def without[X : ClassTag] =
    xs.filter {
      case _: X => false
      case _    => true
    }


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
