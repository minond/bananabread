package bananabread
package runtime.memory

import runtime.value.{Value, Nullptr, I32}

import scala.collection.mutable.ListBuffer


class Stack():
  private val data = ListBuffer.empty[Value]

  def set(i: I32, v: Value): Unit =
    set(i.value, v)
  def set(i: Int, v: Value): Unit =
    ensureAccessible(i)
    data.update(i, v)

  def get(i: I32): Value =
    get(i.value)
  def get(i: Int): Value =
    ensureAccessible(i)
    data(i)

  def ensureAccessible(i: Int): Unit =
    if i >= data.size then
      growBy(1)
      ensureAccessible(i)

  def growBy(size: Int) =
    data.appendAll(List.fill(size)(Nullptr))

  override def toString: String =
    data.mkString(", ")