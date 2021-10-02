package bananabread
package ir
package linked
package error


sealed trait LiftErr
case class UndeclaredIdentifierErr(id: typeless.Id) extends LiftErr
