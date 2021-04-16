package sourdough
package opcode


sealed trait Opcode
case object Halt extends Opcode
case object Label extends Opcode
case object PushI32 extends Opcode
case object Call extends Opcode
