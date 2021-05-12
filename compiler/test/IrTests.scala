package bananabread
package test

import org.scalatest._
import flatspec._
import matchers._

import parsing.lang.parse
import parsing.syntax.Syntax


class IrTests extends AnyFlatSpec with should.Matchers:
  def typelessIrOf(code: String, syntax: Syntax = stdOps) =
    ir.typeless.lift(parse("<test>", code, syntax).getOrElse(???).nodes.head).toString

  it should "typeless ir converts operators into function applications" in {
    typelessIrOf("1 + 2") shouldEqual "(app lambda: (id +) args: ((num 1) (num 2)))"
  }
