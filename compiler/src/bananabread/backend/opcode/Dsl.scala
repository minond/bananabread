package bananabread
package backend.opcode
package dsl

import runtime.register => reg
import runtime.register._
import runtime.instruction._
import runtime.value


val ret = Ret
val swap = Swap
val concat = Concat
val halt = Halt
val println_ = Println
val call0 = Call0

def frame_init(size: Int) = FrameInit(size)
def label_(label: String) = Label(label)

object stw:
  val Rt = Stw(reg.Rt)
  val Ebp = Stw(reg.Ebp)
  val Esp = Stw(reg.Esp)

object ldw:
  val Rt = Ldw(reg.Rt)
  val Ebp = Ldw(reg.Ebp)
  val Esp = Ldw(reg.Esp)

object mov:
  val Pc = Mov(reg.Pc, None)
  val Lr = Mov(reg.Lr, None)
  val Jm = Mov(reg.Jm, None)
  val Rt = Mov(reg.Rt, None)

  def Pc(v: value.I32) = Mov(reg.Pc, Some(v))
  def Lr(v: value.I32) = Mov(reg.Lr, Some(v))
  def Jm(v: value.I32) = Mov(reg.Jm, Some(v))
  def Rt(v: value.I32) = Mov(reg.Rt, Some(v))

  def Pc(v: Int) = Mov(reg.Pc, Some(value.I32(v)))
  def Lr(v: Int) = Mov(reg.Lr, Some(value.I32(v)))
  def Jm(v: Int) = Mov(reg.Jm, Some(value.I32(v)))
  def Rt(v: Int) = Mov(reg.Rt, Some(value.I32(v)))
