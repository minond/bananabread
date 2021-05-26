package bananabread
package runtime.instruction

import runtime.value
import runtime.value.Value
import runtime.register.Register


sealed trait Type
case object I32 extends Type
case object Str extends Type
case object Reg extends Type
case object Ref extends Type
case object Symbol extends Type
case object Scope extends Type
case object Const extends Type


case class Label(label: String)
case class Value(typ: Type, label: String, _value: value.Value)


sealed trait Instruction
case object Halt extends Instruction
case class Jz(label: String) extends Instruction
case class Jmp(label: String) extends Instruction
case class Push(typ: Type, _value: value.Value) extends Instruction
case class Call(label: String) extends Instruction
case object Call0 extends Instruction
case object Ret extends Instruction
case object Swap extends Instruction
case class Mov(reg: Register, data: Option[value.I32]) extends Instruction
case class Load(typ: Type, label: String) extends Instruction
case class Store(typ: Type, label: String) extends Instruction
case object Println extends Instruction
case object Concat extends Instruction
case class Add(typ: Type) extends Instruction
case class Sub(typ: Type) extends Instruction
case class Frame(stack: Int, locals: Int) extends Instruction
case object FrameInit extends Instruction


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
    case (op: Value, _) => pp(op)
    case (op: Label, i) => f"${i}%016d    ${pp(op)}"
    case (op: Instruction, i) => f"${i}%016d        ${pp(op)}"
  }.mkString("\n")
def pp(code: Code): String = code match
  case Label(l)          => s"$l:"
  case Value(t, l, v)    => s".$t $l:\n                          $v"
  case Halt              => "halt"
  case Call0             => "call0"
  case Ret               => "ret"
  case Swap              => "swap"
  case Println           => "println"
  case Concat            => "concat"
  case FrameInit         => "frame_init"
  case Jz(l)             => s"jz        $l"
  case Jmp(l)            => s"jmp       $l"
  case Push(t, v)        => s"push      $t, $v"
  case Call(l)           => s"call      $l"
  case Mov(reg, None)    => s"mov       $reg"
  case Mov(reg, Some(v)) => s"mov       $reg, $v"
  case Load(t, l)        => s"load      $t, $l"
  case Store(t, l)       => s"store     $t, $l"
  case Add(t)            => s"add       $t"
  case Sub(t)            => s"sub       $t"
  case Frame(stack, locals) => s"frame     $stack, $locals"
