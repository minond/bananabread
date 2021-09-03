package bananabread
package runtime
package register

import value.{Value, I32, Pointer, Ptr, Nullptr}


sealed trait Register
case object Pc extends Register
case object Esp extends Register // ESP is the current stack pointer
case object Ebp extends Register // EBP is the base pointer for the current stack frame
case object Lr extends Register
case object Jm extends Register
case object Rt extends Register


class Registers:
  private var pcVal  = I32(0)
  private var espVal = I32(0)
  private var ebpVal = I32(0)
  private var lrVal  = I32(0)
  private var jmVal  = I32(0)
  private var rtVal: Value = Nullptr

  def pc          = pcVal
  def pc(v: I32)  = pcVal = v
  def pc(i: Int)  = pcVal = I32(i)

  def esp         = espVal
  def esp(v: I32) = espVal = v
  def esp(i: Int) = espVal = I32(i)

  def ebp         = ebpVal
  def ebp(v: I32) = ebpVal = v
  def ebp(i: Int) = ebpVal = I32(i)

  def lr          = lrVal
  def lr(v: I32)  = lrVal = v
  def lr(i: Int)  = lrVal = I32(i)

  def jm          = jmVal
  def jm(v: I32)  = jmVal = v
  def jm(i: Int)  = jmVal = I32(i)

  def rt            = rtVal
  def rt(v: Value)  = rtVal = v

  def inc(reg: Register) = reg match
    case Rt  => throw new Exception("invalid operation on rt register")
    case Pc  => pc(pcVal.value + 1)
    case Esp => esp(espVal.value + 1)
    case Ebp => ebp(ebpVal.value + 1)
    case Lr  => lr(lrVal.value + 1)
    case Jm  => jm(jmVal.value + 1)

  def dec(reg: Register) = reg match
    case Rt  => throw new Exception("invalid operation on rt register")
    case Pc  => pc(pcVal.value - 1)
    case Esp => esp(espVal.value - 1)
    case Ebp => ebp(ebpVal.value - 1)
    case Lr  => lr(lrVal.value - 1)
    case Jm  => jm(jmVal.value - 1)

  def get(reg: Register): I32 = reg match
    case Rt  => throw new Exception("invalid operation on rt register")
    case Pc  => pc
    case Esp => esp
    case Ebp => ebp
    case Lr  => lr
    case Jm  => jm

  def set(reg: Register, v: Int): Unit = set(reg, I32(v))
  def set(reg: Register, v: I32): Unit = reg match
    case Rt  => throw new Exception("invalid operation on rt register")
    case Pc  => pc(v)
    case Esp => esp(v)
    case Ebp => ebp(v)
    case Lr  => lr(v)
    case Jm  => jm(v)

  override def toString: String =
    f"pc => ${pc.value}, esp => $esp, ebp -> $ebp, lr => ${lr.value}, jm => ${jm.value}, rt => $rt"
