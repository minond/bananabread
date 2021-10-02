package bananabread
package ir
package typed
package error


sealed trait LiftErr
case class UndeclaredIdentifierErr(id: linked.Id) extends LiftErr
