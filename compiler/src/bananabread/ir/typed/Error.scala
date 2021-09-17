package bananabread
package ir
package typed
package error


sealed trait LiftErr
case class UndeclaredIdentifierErr(id: typeless.Id) extends LiftErr
