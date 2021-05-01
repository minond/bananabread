package bananabread
package utils

import scala.reflect.ClassTag


trait Print(inner: String = ""):
  self =>
    override def toString =
      if inner == ""
      then getClass.getSimpleName.toUpperCase
      else inner


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
