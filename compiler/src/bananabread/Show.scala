package bananabread
package show

import scala.collection.mutable.Map


trait Print(inner: String = ""):
  self =>
    override def toString =
      if inner == ""
      then getClass.getSimpleName.toUpperCase
      else inner


val ids = LazyList.from(1).sliding(1)
val ptrs = Map[String, String]()
def ptrOf(header: String, hash: Int) = ptrs.get(header + hash.toString) match
  case Some(ptr) => ptr
  case None =>
    val ptr = s"$header-${ids.next.head}"
    ptrs.update(header + hash.toString, ptr)
    ptr

trait Ptr(prefix: String):
  def ptr = ptrOf(prefix, hashCode)
