package bananabread
package printer
package instructions

import runtime.instruction._
import runtime.value


def pp(codes: List[Code]): String =
  codes.zipWithIndex.map {
    case (op: Data, _) => pp(op)
    case (op: Label, i) => pp(op)
    case (op: Instruction, i) => (" " * 8) + pp(op)
  }.mkString("\n")
def pp(code: Code, align: Boolean = true): String = code match
  case Label(l)          => s"$l:"
  case Halt              => "halt"
  case Call0             => "call0"
  case Ret               => "ret"
  case Swap              => "swap"
  case Println           => "println"
  case Concat            => "concat"
  case Jz(l)             => if align then s"jz        $l"              else s"jz $l"
  case Jmp(l)            => if align then s"jmp       $l"              else s"jmp $l"
  case Push(t, v)        => if align then s"push      $t, $v"          else s"push $t, $v"
  case Call(l)           => if align then s"call      $l"              else s"call $l"
  case Stw(reg)          => if align then s"stw       $reg"            else s"stw $reg"
  case Ldw(reg)          => if align then s"ldw       $reg"            else s"ldw $reg"
  case Load(t, l)        => if align then s"load      $t, $l"          else s"load $t, $l"
  case Store(t, l)       => if align then s"store     $t, $l"          else s"store $t, $l"
  case Add(t)            => if align then s"add       $t"              else s"add $t"
  case Sub(t)            => if align then s"sub       $t"              else s"sub $t"
  case Frame(argc)       => if align then s"frame     $argc" else s"frame $argc"
  case FrameInit(argc)   => if align then s"frame!    $argc" else s"frame! $argc"

  case Mov(reg, None, None, None)               => if align then s"mov       $reg"                  else s"mov $reg"
  case Mov(reg, None, Some(v), None)            => if align then s"mov       $reg, $v"              else s"mov $reg, $v"
  case Mov(reg, Some(addr), None, None)         => if align then s"mov       $reg, %$addr"          else s"mov $reg, %$addr"
  case Mov(reg, Some(addr), Some(offset), None) => if align then s"mov       $reg, $offset(%$addr)" else s"mov $reg, $offset(%$addr)"
  case Mov(reg, Some(addr), None, Some(offset)) => if align then s"mov       $reg, %$addr+$offset"  else s"mov $reg, %$addr+$offset"
  case Mov(reg, _, _, _)                        => if align then s"mov       $reg, <invalid>"       else s"mov $reg, <invalid>"

  case Data(t, l, v) =>
    v match
      case lista: value.Lista =>
        val sb = StringBuilder()
        sb.append(s"$l:")
        sb.append("\n")
        sb.append(" " * 8)
        sb.append(s".size     ${lista.size}")
        for (item <- lista.items)
          sb.append("\n")
          sb.append(" " * 8)
          item match
            case i: value.I32 => sb.append(s".${I32.toString.padTo(8, ' ')} $i")
            case i: value.Str => sb.append(s"${Str.toString.padTo(8, ' ')} $i")
            case i: value.Id => sb.append(s"${Ref.toString.padTo(8, ' ')} $i")
            case i: value.Scope => sb.append(s"${Ref.toString.padTo(8, ' ')} $i")
            case i: value.Symbol => sb.append(s"${Symbol.toString.padTo(8, ' ')} $i")
            case i: value.Lista => sb.append(s"${Lista.toString.padTo(8, ' ')} $i")
            case value.True => sb.append(s"${Bool.toString.padTo(8, ' ')} $v")
            case value.False => sb.append(s"${Bool.toString.padTo(8, ' ')} $v")
            case value.Nullptr => sb.append(s"${Ref.toString.padTo(8, ' ')} $v")
            case i: value.Ptr => sb.append(s"${Ref.toString.padTo(8, ' ')} $i")
        sb.toString
      case _ =>
        s"$l:\n" + f"${" " * 8}.${t.toString.padTo(8, ' ')} $v"
