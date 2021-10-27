package bananabread
package runtime
package memory

import value.{Value, Ptr, Nullptr, I32, Lista}

import scala.collection.mutable.{ListBuffer, Map => MutableMap}


class Heap(constants: Map[String, Value]) extends Memory:
  private[this] var curr = 0
  private[this] var map = MutableMap.empty[String, Ptr]

  constants.foreach {
    case (name, value) => store(name, value)
  }

  def malloc(size: Int): Ptr =
    val ptr = Ptr(curr)
    val end = curr + size
    ensureAccessible(end)
    curr = end
    ptr

  def store(v: Value): Ptr = v match
    case lista: Lista =>
      val ptr = malloc(lista.size)
      lista.items.zipWithIndex.foreach {
        case (v, i) =>
          data.update(ptr.addr + i, v)
      }
      ptr
    case _ =>
      val ptr = malloc(v.size)
      data.update(ptr.addr, v)
      ptr

  def store(label: String, v: Value): Ptr =
    val ptr = store(v)
    map.update(label, ptr)
    ptr

  def lookup(ptr: Ptr): Value =
    ensureAccessible(ptr.addr)
    data(ptr.addr)

  def lookup(label: String): Option[Value] =
    map.get(label) match
      case None => None
      case Some(ptr) => Some(lookup(ptr))


class Stack() extends Memory:
  def set(i: I32, v: Value): Unit = set(i.value, v)
  def set(i: Int, v: Value): Unit =
    ensureAccessible(i)
    data.update(i, v)

  def get(i: I32): Value = get(i.value)
  def get(i: Int): Value =
    ensureAccessible(i)
    data(i)


trait Memory:
  private[memory] val data = ListBuffer.empty[Value]

  override def toString: String =
    data.mkString(", ")

  private[memory] def ensureAccessible(i: Int): Unit =
    if i >= data.size then
      growBy(1)
      ensureAccessible(i)

  private[memory] def growBy(size: Int) =
    data.appendAll(List.fill(size)(Nullptr))
