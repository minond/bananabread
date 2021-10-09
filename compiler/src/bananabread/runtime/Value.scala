package bananabread
package runtime
package value

import utils.Print


sealed trait Value
case class I32(value: Int) extends Value with Print(value.toString)
case class Str(value: String) extends Value with Print(value)
case class Id(label: String) extends Value with Print(label)
case class Scope(label: String, frame: Frame) extends Value with Print(s"<$label>")
case class Symbol(value: String) extends Value with Print(value)

sealed trait Bool extends Value
case object True extends Bool with Print("true")
case object False extends Bool with Print("false")

sealed trait Pointer extends Value
case class Ptr(addr: Int) extends Pointer with Print(s"<$addr>")
case object Nullptr extends Pointer with Print("nullptr")


