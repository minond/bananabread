package bananabread
package runtime

import value.Value
import instruction._
import register._


def handle(code: Code, state: State): Dispatch = code match
  case _: Data   => Cont
  case _: Label  => Cont
  case Halt      => Stop
  case op: FrameInit => Error("unknown operator", op)
  case op: Jz    => handleJz(op, state)
  case op: Jmp   => handleJmp(op, state)
  case op: Push  => handlePush(op, state)
  case op: Call  => handleCall(op, state)
  case Call0     => handleCall0(Call0, state)
  case Ret       => handleRet(Ret, state)
  case Swap      => handleSwap(Swap, state)
  case op: Mov   => handleMov(op, state)
  case op: Stw   => handleStw(op, state)
  case op: Ldw   => handleLdw(op, state)
  case op: Load  => handleLoad(op, state)
  case op: Store => handleStore(op, state)
  case Println   => handlePrintln(Println, state)
  case Concat    => handleConcat(Concat, state)
  case op: Add   => handleAdd(op, state)
  case op: Sub   => handleSub(op, state)
  case op: Frame => handleFrame(op, state)

def handleJz(op: Jz, state: State): Dispatch = state.pop match
  case value.I32(0) => goto(op, op.label, state)
  case _            => Cont

def handleJmp(op: Jmp, state: State): Dispatch =
  goto(op, op.label, state)

def handlePush(op: Push, state: State): Dispatch = op match
  case Push(I32, v: value.I32) =>
    state.push(v)
    Cont
  case Push(Bool, v: value.Bool) =>
    state.push(v)
    Cont
  case Push(Ref, v: value.Id) =>
    state.push(v)
    Cont
  case Push(Scope, value.Id(label)) =>
    val scope = value.Scope(label, state.frames.curr)
    state.push(scope)
    Cont
  case Push(Const, value.Id(label)) =>
    const(op, label, state) { const =>
      state.push(const)
      Cont
    }
  case Push(Ptr, value.Id(label)) =>
    state.heap.mapped(label) match
      case None =>
        Error("bad push: invalid pointer", op)
      case Some(ptr) =>
        state.push(value.I32(ptr.addr))
        Cont
  case _ =>
    Error("bad push", op)

def handleCall(op: Call, state: State): Dispatch = state.frames.curr.get(op.label) match
  case None =>
    state.push(value.I32(state.registers.pc.value + 1))
    state.frames.next
    goto(op, op.label, state)
  case Some(ptr: value.Id) =>
    state.push(value.I32(state.registers.pc.value + 1))
    state.frames.next
    goto(op, ptr.label, state)
  case Some(value.Scope(label, frame)) =>
    state.push(value.I32(state.registers.pc.value + 1))
    state.frames.from(frame)
    goto(op, label, state)
  case Some(bad) =>
    Error(s"bad call: ${op.label} is instance of `${bad.getClass.getSimpleName}`, expected function", op)

def handleCall0(op: Instruction, state: State): Dispatch =
  state.push(value.I32(state.registers.pc.value + 1))
  Jump(state.registers.jm.value)

def handleRet(op: Instruction, state: State): Dispatch = state.pop match
  case value.I32(addr) =>
    state.frames.prev
    Jump(addr)
  case bad =>
    Error(s"bad ret: missing return address: $bad", op)

def handleSwap(op: Instruction, state: State): Dispatch =
  if state.registers.esp.value == 0
  then Cont
  else
    val a = state.pop
    val b = state.pop
    state.push(a)
    state.push(b)
    Cont

def handleMov(op: Mov, state: State): Dispatch = op match
  case Mov(reg, Some(addr), Some(offset), None) =>
    state.heap.mapped(addr.label) match
      case Some(ptr) =>
        val data = state.heap.lookup(ptr.addr + offset.value)
        state.registers.setValue(reg, data)
        Cont
      case None =>
        Error(s"bad mov: missing label ${addr.label}", op)
  case Mov(reg, None, Some(offset), None) =>
    ???
  case Mov(reg, Some(addr), None, Some(offsetReg)) =>
    (state.heap.mapped(addr.label), state.registers.getValue(offsetReg)) match
      case (Some(ptr), offset: value.I32) =>
        val data = state.heap.lookup(ptr.addr + offset.value)
        state.registers.setValue(reg, data)
        Cont
      case (Some(ptr), v) =>
        Error(s"bad mov: ${offsetReg} register did not contain a valid offset", op)
      case (None, _) =>
        Error(s"bad mov: missing label ${addr.label}", op)
  case Mov(reg, None, None, None) => state.pop match
    case addr: value.I32 =>
      state.registers.set(reg, addr)
      Cont
    case value.Id(label) =>
      state.labels.get(label) match
        case Some(i) =>
          state.registers.set(reg, i)
          Cont
        case None =>
          Error(s"bad mov: missing label $label", op)
    case value.Scope(label, frame) =>
      state.labels.get(label) match
        case Some(i) =>
          state.frames.from(frame)
          state.registers.set(reg, value.I32(i))
          Cont
        case None =>
          Error(s"bad mov: missing scope $label", op)
    case _ =>
      Error("bad mov: invalid stack entry", op)

def handleStw(op: Stw, state: State): Dispatch =
  op.reg match
    case Rt =>
      state.push(state.registers.rt)
      Cont
    case Ebp =>
      state.push(state.registers.ebp)
      Cont
    case Esp =>
      state.push(state.registers.esp)
      Cont
    case Rax =>
      state.push(state.registers.rax)
      Cont
    case _ =>
      Error("bad stw: register does not accept user data", op)

def handleLdw(op: Ldw, state: State): Dispatch =
  (op.reg, state.pop) match
    case (Rt | Rax, v) =>
      state.registers.setValue(op.reg, v)
      Cont
    case (Ebp, v : value.I32) =>
      state.registers.ebp(v.value)
      Cont
    case (Esp, v : value.I32) =>
      state.registers.esp(v.value)
      Cont
    case (Ebp | Esp, _) =>
      Error("bad ldw: register does not accept non-int32 data", op)
    case (_, _) =>
      Error("bad ldw: register does not accept user data", op)

def handleLoad(op: Load, state: State): Dispatch = state.frames.curr.get(op.label) match
  case None =>
    pointer(op, op.label, state) { value =>
      state.push(value)
      Cont
    }
  case Some(v) =>
    state.push(v)
    Cont

def handleStore(op: Store, state: State): Dispatch =
  state.frames.curr.put(op.label, state.pop)
  Cont

def handlePrintln(ins: Instruction, state: State): Dispatch =
  println(state.pop)
  Cont

def handleConcat(op: Instruction, state: State): Dispatch =
  binStrOp(state)(_ + _) match
    case Some(v) =>
      state.push(v)
      Cont
    case None =>
      Error("bad concat: missing argument", op)

def handleAdd(op: Add, state: State): Dispatch =
  binI32Op(state)(_ + _) match
    case Some(v) =>
      state.push(v)
      Cont
    case None =>
      Error("bad add: missing argument", op)

def handleSub(op: Sub, state: State): Dispatch =
  binI32Op(state)(_ - _) match
    case Some(v) =>
      state.push(v)
      Cont
    case None =>
      Error("bad sub: missing argument", op)

def handleFrame(op: Frame, state: State): Dispatch =
  Cont

def goto(op: Instruction, label: String, state: State): Dispatch = state.labels.get(label) match
  case None => Error(s"bad jump: missing label: ${label}", op)
  case _ => Goto(label)

def const(op: Instruction, label: String, state: State)(f: Value => Dispatch): Dispatch =
  state.heap.lookup(label) match
    case None => Error(s"missing const: ${label}", op)
    case Some(value) => f(value)

def pointer(op: Instruction, label: String, state: State)(f: Value => Dispatch): Dispatch =
  (state.heap.lookup(label), state.labels.get(label)) match
    case (None, None) => Error(s"invalid pointer: ${label}", op)
    case (Some(value), _) => f(value)
    case (_, Some(_)) => f(value.Id(label))

def binI32Op(state: State)(f: (Int, Int) => Int): Option[value.I32] =
  (state.pop, state.pop) match
    case (value.I32(rhs), value.I32(lhs)) =>
      Some(value.I32(f(lhs, rhs)))
    case _ =>
      None

def binStrOp(state: State)(f: (String, String) => String): Option[value.Str] =
  (state.pop, state.pop) match
    case (value.Str(rhs), value.Str(lhs)) =>
      Some(value.Str(f(lhs, rhs)))
    case _ =>
      None
