package sourdough
package test

import org.scalatest._
import flatspec._
import matchers._

import parser.{parse, Syntax}


class Ir1Tests extends AnyFlatSpec with should.Matchers:
  def ir1Of(code: String, syntax: Syntax = stdOps) =
    ir1.lift(parse("<test>", code, syntax).getOrElse(???).head).toString

  it should "convert operators into function applications" in {
    ir1Of("1 + 2") shouldEqual "(app lambda: (id +) args: ((num 1) (num 2)))"
  }
