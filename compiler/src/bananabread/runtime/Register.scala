package bananabread
package runtime.register


sealed trait Register
case object Pc extends Register
case object Lr extends Register
case object Jmp extends Register
