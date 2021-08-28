package bananabread
package runtime.register

import runtime.value.{Value, I32, Pointer, Ptr, Nullptr}


sealed trait Register
case object Pc extends Register
case object Sp extends Register
case object Lr extends Register
case object Jm extends Register
case object Rt extends Register


class Registers:
  private var pcVal = I32(0)
  private var spVal = I32(0)
  private var lrVal = I32(0)
  private var jmVal = I32(0)
  private var rtVal: Value = Nullptr

  def pc         = pcVal
  def pc(v: I32) = pcVal = v
  def pc(i: Int) = pcVal = I32(i)

  def sp         = spVal
  def sp(v: I32) = spVal = v
  def sp(i: Int) = spVal = I32(i)

  def lr         = lrVal
  def lr(v: I32) = lrVal = v
  def lr(i: Int) = lrVal = I32(i)

  def jm         = jmVal
  def jm(v: I32) = jmVal = v
  def jm(i: Int) = jmVal = I32(i)

  def rt           = rtVal
  def rt(v: Value) = rtVal = v

  def inc(reg: Register) = reg match
    case Pc => pc(pcVal.value + 1)
    case Sp => sp(spVal.value + 1)
    case Lr => lr(lrVal.value + 1)
    case Jm => jm(jmVal.value + 1)

  def dec(reg: Register) = reg match
    case Pc => pc(pcVal.value - 1)
    case Sp => sp(spVal.value - 1)
    case Lr => lr(lrVal.value - 1)
    case Jm => jm(jmVal.value - 1)

  def get(reg: Register): I32 = reg match
    case Pc => pc
    case Sp => sp
    case Lr => lr
    case Jm => jm

  def set(reg: Register, v: Int): Unit = set(reg, I32(v))
  def set(reg: Register, v: I32): Unit = reg match
    case Pc => pc(v)
    case Sp => sp(v)
    case Lr => lr(v)
    case Jm => jm(v)

  override def toString: String =
    f"pc => ${pc.value}, sp => $sp, lr => ${lr.value}, jm => ${jm.value}, rt => $rt"
