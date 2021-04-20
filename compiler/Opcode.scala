package sourdough
package opcode

import utils.Print


sealed trait Opcode
case object Halt extends Opcode with Print("halt")
case object Label extends Opcode
case object Jz extends Opcode with Print("jz")
case object Jmp extends Opcode with Print("jmp")
case object PushI32 extends Opcode with Print("push_i32")
case object PushPtr extends Opcode with Print("push_ptr")
case object PushReg extends Opcode with Print("push_reg")
case object Call extends Opcode with Print("call")
case object Run extends Opcode with Print("run")
case object Ret extends Opcode with Print("ret")
case object Swap extends Opcode with Print("swap")
case object Mov extends Opcode with Print("mov")
case object LoadI32 extends Opcode with Print("load_i32") // local to stack
case object StoreI32 extends Opcode with Print("store_i32") // stack to local
case object StorePtr extends Opcode with Print("store_ptr") // stack to local
