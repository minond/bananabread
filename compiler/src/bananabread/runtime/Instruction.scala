package bananabread
package runtime.instruction

import runtime.value
import runtime.value.Value
import runtime.register.Register
import utils.Print


sealed trait Type
case object I32 extends Type
case object Str extends Type
case object Reg extends Type
case object Ptr extends Type
case object Symbol extends Type
case object Scope extends Type
case object Const extends Type


case class Label(label: String)
case class Value(typ: Type, label: String, _value: value.Value)


sealed trait Instruction
case object Halt extends Instruction with Print("halt")
case class Jz(label: String) extends Instruction with Print(s"jz        $label")
case class Jmp(label: String) extends Instruction with Print(s"jmp       $label")
case class Push(typ: Type, _value: value.Value) extends Instruction with Print(s"push      $typ, $_value")
case class Call(label: String) extends Instruction with Print(s"call      $label")
case object Call0 extends Instruction with Print("call0")
case object Ret extends Instruction with Print("ret")
case object Swap extends Instruction with Print("swap")
case class Mov(reg: Register, data: Option[value.I32]) extends Instruction with Print(s"mov       ${reg.toString + data.map(", " + _).getOrElse("")}")
case class Load(typ: Type, label: String) extends Instruction with Print(s"load      $typ, $label")
case class Store(typ: Type, label: String) extends Instruction with Print(s"store     $typ, $label")
case object Println extends Instruction with Print("println")
case object Concat extends Instruction with Print("concat")
case class Add(typ: Type) extends Instruction with Print(s"add       $typ")
case class Sub(typ: Type) extends Instruction with Print(s"sub       $typ")
case class Frame(stack: Int, locals: Int) extends Instruction with Print(s"frame     $stack, $locals")
case object FrameInit extends Instruction with Print(s"frame_init")


type Code = Label
          | Value
          | Instruction

extension (codes: List[Code])
  def labels: Map[String, Int] =
    codes.zipWithIndex.collect {
      case (Label(label), index) => (label, index)
    }.toMap

  def constants: Map[String, value.Value] =
    codes.collect {
      case Value(_, label, value) => (label, value)
    }.toMap


def pp(codes: List[Code]): String =
  codes.zipWithIndex.map {
    case (Label(label), i) => s"$label:"
    case (Value(typ, label, value), i) => s"$label [$typ]: $value"
    case (op : Instruction, i) => f"${i}%016d          ${op}"
  }.mkString("\n")
