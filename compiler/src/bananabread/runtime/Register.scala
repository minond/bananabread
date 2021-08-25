package bananabread
package runtime.register

import runtime.value.{Value, I32, Pointer, Ptr, Nullptr}


sealed trait Register
case object Pc extends Register
case object Lr extends Register
case object Jm extends Register
case object Rt extends Register


class Registers:
  private var pcVal = I32(0)
  private var lrVal = I32(0)
  private var jmVal = I32(0)
  private var rtVal: Value = Nullptr

  def pc         = pcVal
  def pc(v: I32) = pcVal = v
  def pc(i: Int) = pcVal = I32(i)

  def lr         = lrVal
  def lr(v: I32) = lrVal = v
  def lr(i: Int) = lrVal = I32(i)

  def jm         = jmVal
  def jm(v: I32) = jmVal = v
  def jm(i: Int) = jmVal = I32(i)

  def rt           = rtVal
  def rt(v: Value) = rtVal = v

  def get(reg: Register): I32 = reg match
    case Pc => pc
    case Lr => lr
    case Jm => jm

  def set(reg: Register, v: Int): Unit = set(reg, I32(v))
  def set(reg: Register, v: I32): Unit = reg match
    case Pc => pc(v)
    case Lr => lr(v)
    case Jm => jm(v)

  override def toString: String =
    f"pc => ${pc.value}%08X, lr => ${lr.value}%08X, jm => ${jm.value}%08X, rt => $rt"
