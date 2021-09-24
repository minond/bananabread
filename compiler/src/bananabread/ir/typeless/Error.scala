package bananabread
package ir.typeless
package error

import parsing.ast.{Expr, Stmt}


sealed trait LiftErr
case class LiftinTheUnliftableErr(node: Expr | Stmt) extends LiftErr
