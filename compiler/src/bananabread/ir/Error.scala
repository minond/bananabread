package bananabread
package ir.typeless
package error


sealed trait LiftErr
case class BadParamIdentifier(params: List[Ir]) extends LiftErr
