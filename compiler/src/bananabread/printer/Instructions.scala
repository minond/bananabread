package bananabread
package printer
package instructions


import runtime.instruction._


def pp(codes: List[Code]): String =
  codes.zipWithIndex.map {
    case (op: Value, _) => pp(op)
    case (op: Label, i) => f"${i}%016d    ${pp(op)}"
    case (op: Instruction, i) => f"${i}%016d        ${pp(op)}"
  }.mkString("\n")
def pp(code: Code, align: Boolean = true): String = code match
  case Label(l)             => s"$l:"
  case Value(t, l, v)       => f".$t ${(l + ":").padTo(14, ' ')} $v"
  case Halt                 => "halt"
  case Call0                => "call0"
  case Ret                  => "ret"
  case Swap                 => "swap"
  case Println              => "println"
  case Concat               => "concat"
  case Jz(l)                => if align then s"jz        $l"              else s"jz $l"
  case Jmp(l)               => if align then s"jmp       $l"              else s"jmp $l"
  case Push(t, v)           => if align then s"push      $t, $v"          else s"push $t, $v"
  case Call(l)              => if align then s"call      $l"              else s"call $l"
  case Mov(reg, None)       => if align then s"mov       $reg"            else s"mov $reg"
  case Mov(reg, Some(v))    => if align then s"mov       $reg, $v"        else s"mov $reg, $v"
  case Stw(reg)             => if align then s"stw       $reg"            else s"stw $reg"
  case Ldw(reg)             => if align then s"ldw       $reg"            else s"ldw $reg"
  case Load(t, l)           => if align then s"load      $t, $l"          else s"load $t, $l"
  case Store(t, l)          => if align then s"store     $t, $l"          else s"store $t, $l"
  case Add(t)               => if align then s"add       $t"              else s"add $t"
  case Sub(t)               => if align then s"sub       $t"              else s"sub $t"
  case Frame(argc)          => if align then s"frame     $argc" else s"frame $argc"
  case FrameInit(argc)      => if align then s"frame!    $argc" else s"frame! $argc"
