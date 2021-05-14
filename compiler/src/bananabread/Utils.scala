package bananabread
package utils

import scala.reflect.ClassTag


implicit class ListImplicits[T](xs: List[T]):
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


implicit class ListOfEitherImplicits[L, R](val eithers: List[Either[L, R]]):
  def squished: Either[L, List[R]] =
    EitherImplicits(eithers.iterator).squished

implicit class EitherImplicits[L, R](val eithers: Iterator[Either[L, R]]):
  /** Converts an [[Iterator[Either[L, R]]]] into an [[Either[L, List[R]]]].
   */
  def squished: Either[L, List[R]] =
    eithers.foldLeft[Either[L, List[R]]](Right(List())) {
      (acc, x) =>
        acc.flatMap(xs => x.map(xs :+ _))
    }


implicit class BaseImplicits[T <: Any](x: T):
  def asList: List[T] = List(x)


implicit class ComparisonImplicits(val obj: Any):
  def is[T : ClassTag] = obj match
    case _ : T => true
    case _     => false
