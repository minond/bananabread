package bananabread
package backend.opcode
package dsl

import runtime.register => reg
import runtime.register._
import runtime.instruction._
import runtime.instruction => inst
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
  val Rax = Stw(reg.Rax)

object ldw:
  val Rt = Ldw(reg.Rt)
  val Ebp = Ldw(reg.Ebp)
  val Esp = Ldw(reg.Esp)

object mov:
  val Pc = Mov(reg.Pc, None, None, None)
  val Lr = Mov(reg.Lr, None, None, None)
  val Jm = Mov(reg.Jm, None, None, None)
  val Rt = Mov(reg.Rt, None, None, None)

  def Pc(v: value.I32) = Mov(reg.Pc, None, Some(v), None)
  def Lr(v: value.I32) = Mov(reg.Lr, None, Some(v), None)
  def Jm(v: value.I32) = Mov(reg.Jm, None, Some(v), None)
  def Rt(v: value.I32) = Mov(reg.Rt, None, Some(v), None)

  def Pc(v: Int) = Mov(reg.Pc, None, Some(value.I32(v)), None)
  def Lr(v: Int) = Mov(reg.Lr, None, Some(value.I32(v)), None)
  def Jm(v: Int) = Mov(reg.Jm, None, Some(value.I32(v)), None)
  def Rt(v: Int) = Mov(reg.Rt, None, Some(value.I32(v)), None)

object push:
  def Ref(v: String) = Push(inst.Ref, value.Id(v))
  def Ref(v: value.Id) = Push(inst.Ref, v)
  def Str(v: value.Id) = Push(inst.Str, v)
