package bananabread
package backend
package opcode

import show.Print
import runtime.value.Value
import runtime.register.Register


sealed trait Type
case object I32 extends Type
case object Str extends Type
case object Reg extends Type
case object Ptr extends Type
case object Scope extends Type
case object Const extends Type


sealed trait Instruction
case object Halt extends Instruction with Print("halt")
case class Label(label: String) extends Instruction with Print(s"$label:")
case class Value(typ: Type, label: String, value: Value) extends Instruction with Print(s"$label [$typ]: $value")
case class Jz(label: String) extends Instruction with Print(s"jz $label")
case class Jmp(label: String) extends Instruction with Print(s"jmp $label")
case class Push(typ: Type, value: Value) extends Instruction with Print(s"push [$typ]")
case class Call(label: String) extends Instruction with Print(s"call $label")
case object Call0 extends Instruction with Print("call0")
case object Ret extends Instruction with Print("ret")
case object Swap extends Instruction with Print("swap")
case class Mov(reg: Register) extends Instruction with Print(s"mov $reg")
case class Load(typ: Type, label: String) extends Instruction with Print(s"load [$typ] $label")
case class Store(typ: Type, label: String) extends Instruction with Print(s"store [$typ] $label")
case object Println extends Instruction with Print("println")
case class Add(typ: Type) extends Instruction with Print(s"add [$typ]")
case class Sub(typ: Type) extends Instruction with Print(s"sub [$typ]")
case object Concat extends Instruction with Print("concat")
