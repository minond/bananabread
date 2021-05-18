package bananabread
package runtime

import instruction.{Value => Value_, Label => Label_, _}


def handle(code: Code, state: State): Dispatch = code match
  case _: Value_ => Cont
  case _: Label_ => Cont
  case Halt      => Stop
  case FrameInit => Fatal("unknown operator")
  case op: Jz    => handleJz(op, state)
  case op: Jmp   => handleJmp(op, state)
  case op: Push  => handlePush(op, state)
  case op: Call  => handleCall(op, state)
  case Call0     => handleCall0(state)
  case Ret       => handleRet(state)
  case Swap      => handleSwap(state)
  case op: Mov   => handleMov(op, state)
  case op: Load  => handleLoad(op, state)
  case op: Store => handleStore(op, state)
  case Println   => handlePrintln(state)
  case Concat    => handleConcat(state)
  case op: Add   => handleAdd(op, state)
  case op: Sub   => handleSub(op, state)
  case op: Frame => handleFrame(op, state)

def handleJz(op: Jz, state: State): Dispatch = state.stack.pop match
  case value.I32(0) => goto(op.label, state)
  case _            => Cont

def handleJmp(op: Jmp, state: State): Dispatch =
  goto(op.label, state)

def handlePush(op: Push, state: State): Dispatch = op match
  case Push(I32, v: value.I32) =>
    state.stack.push(v)
    Cont
  case Push(Ptr, v: value.Id) =>
    state.stack.push(v)
    Cont
  case Push(Scope, value.Id(label)) =>
    val scope = value.Scope(label, state.frames.curr)
    state.stack.push(scope)
    Cont
  case Push(Const, value.Id(label)) =>
    state.constants.get(label) match
      case Some(v) =>
        state.stack.push(v)
        Cont
      case None =>
        Fatal(s"missing const: $label")
  case _ =>
    Fatal("bad push")

def handleCall(op: Call, state: State): Dispatch = state.frames.curr.get(op.label) match
  case None =>
    state.frames.next
    goto(op.label, state)
  case Some(ptr: value.Id) =>
    state.frames.next
    goto(ptr.label, state)
  case Some(value.Scope(label, frame)) =>
    state.frames.from(frame)
    goto(label, state)
  case Some(_) =>
    Fatal(s"bad call: ${op.label}")

def handleCall0(state: State): Dispatch =
  Jump(state.registers.jm.value)

def handleRet(state: State): Dispatch = state.stack.pop match
  case value.I32(addr) =>
    state.frames.prev
    Jump(addr)
  case _ =>
    Fatal("bad ret: missing return address")

def handleSwap(state: State): Dispatch =
  val a = state.stack.pop
  val b = state.stack.pop
  state.stack.push(a)
  state.stack.push(b)
  Cont

def handleMov(op: Mov, state: State): Dispatch = op match
  case Mov(reg, Some(offset)) =>
    val curr = state.registers.get(reg)
    val next = value.I32(curr.value + offset.value)
    state.stack.push(next)
    Cont
  case Mov(reg, None) => state.stack.pop match
    case addr: value.I32 =>
      state.registers.set(reg, addr)
      Cont
    case value.Id(label) =>
      state.labels.get(label) match
        case Some(i) =>
          state.registers.set(reg, i)
          Cont
        case None =>
          Fatal(s"bad mov: missing label $label")
    case value.Scope(label, frame) =>
      state.labels.get(label) match
        case Some(i) =>
          state.frames.from(frame)
          state.registers.set(reg, value.I32(i))
          Cont
        case None =>
          Fatal(s"bad mov: missing scope $label")
    case _ =>
      Fatal("bad mov: invalid stack entry")

def handleLoad(op: Load, state: State): Dispatch = state.frames.curr.get(op.label) match
  case None =>
    val value = state.constants(op.label)
    state.stack.push(value)
    Cont
  case Some(v) =>
    state.stack.push(v)
    Cont

def handleStore(op: Store, state: State): Dispatch =
  state.frames.curr.put(op.label, state.stack.pop)
  Cont

def handlePrintln(state: State): Dispatch =
  println(state.stack.pop)
  Cont

def handleConcat(state: State): Dispatch =
  binStrOp(state)(_ + _) match
    case Some(v) =>
      state.stack.push(v)
      Cont
    case None =>
      Fatal("bad concat: missing argument")

def handleAdd(op: Add, state: State): Dispatch =
  binI32Op(state)(_ + _) match
    case Some(v) =>
      state.stack.push(v)
      Cont
    case None =>
      Fatal("bad add: missing argument")

def handleSub(op: Sub, state: State): Dispatch =
  binI32Op(state)(_ - _) match
    case Some(v) =>
      state.stack.push(v)
      Cont
    case None =>
      Fatal("bad sub: missing argument")

def handleFrame(op: Frame, state: State): Dispatch =
  Cont

def goto(label: String, state: State): Dispatch = state.labels.get(label) match
  case None => Fatal(s"bad jump: missing label: ${label}")
  case _ => Goto(label)

def binI32Op(state: State)(f: (Int, Int) => Int): Option[value.I32] =
  (state.stack.pop, state.stack.pop) match
    case (value.I32(rhs), value.I32(lhs)) =>
      Some(value.I32(f(lhs, rhs)))
    case _ =>
      None

def binStrOp(state: State)(f: (String, String) => String): Option[value.Str] =
  (state.stack.pop, state.stack.pop) match
    case (value.Str(rhs), value.Str(lhs)) =>
      Some(value.Str(f(lhs, rhs)))
    case _ =>
      None
