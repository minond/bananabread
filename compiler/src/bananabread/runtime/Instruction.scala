package bananabread
package runtime.instruction

import runtime.value
import runtime.value.Value
import runtime.register.Register
import show.Print


sealed trait Type
case object I32 extends Type
case object Str extends Type
case object Reg extends Type
case object Ptr extends Type
case object Symbol extends Type
case object Scope extends Type
case object Const extends Type


type Code = Label
          | Value
          | Instruction


case class Label(label: String) extends Print(s"$label:")
case class Value(typ: Type, label: String, _value: value.Value) extends Print(s"$label [$typ]: $_value")


sealed trait Instruction
case object Halt extends Instruction with Print("halt")
case class Jz(label: String) extends Instruction with Print(s"jz $label")
case class Jmp(label: String) extends Instruction with Print(s"jmp $label")
case class Push(typ: Type, value: String) extends Instruction with Print(s"push [$typ] $value")
case class Call(label: String) extends Instruction with Print(s"call $label")
case object Call0 extends Instruction with Print("call0")
case object Ret extends Instruction with Print("ret")
case object Swap extends Instruction with Print("swap")
case class Mov(reg: Register, data: Option[String]) extends Instruction with Print(s"mov $reg ${data.getOrElse("")}")
case class Load(typ: Type, label: String) extends Instruction with Print(s"load [$typ] $label")
case class Store(typ: Type, label: String) extends Instruction with Print(s"store [$typ] $label")
case object Println extends Instruction with Print("println")
case class Add(typ: Type) extends Instruction with Print(s"add [$typ]")
case class Sub(typ: Type) extends Instruction with Print(s"sub [$typ]")
case object Concat extends Instruction with Print("concat")


def pp(codes: List[Code]): String =
  codes.zipWithIndex.map { (code, i) =>
    f"${i}%05d    ${pp(code)}"
  }.mkString("\n")
def pp(code: Code): String = code match
  case _ : Label       => code.toString
  case _ : Value       => code.toString
  case _ : Instruction => s"  $code"
