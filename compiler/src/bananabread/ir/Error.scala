package bananabread
package ir.typeless
package error


sealed trait LiftErr
case class BadParamIdErr(params: List[Ir]) extends LiftErr
