package bananabread
package ir
package stitched
package error


sealed trait LiftErr
case class MissingSourceErr(id: typed.Id) extends LiftErr
case class ExpectedSingleLiftedMore(node: typed.Ir) extends LiftErr
