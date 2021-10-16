package bananabread
package runtime
package memory

import value.{Value, Ptr, Nullptr, I32}

import scala.collection.mutable.ListBuffer


class Heap() extends Memory:
  private[this] var curr = 0

  def alloc(size: Int): Ptr =
    curr = curr + size
    ensureAccessible(curr)
    Ptr(curr)

  def lookup(ptr: Ptr): Value =
    ensureAccessible(ptr.addr)
    data(ptr.addr)


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
