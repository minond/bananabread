package bananabread
package test

import org.scalatest._
import flatspec._
import matchers._

import parsing.lang.{parse, Syntax}


class ParserTests extends AnyFlatSpec with should.Matchers:
  it should "parse numbers, characters, words" in {
    exprsOf("1 2 3 a b c + - * testing123") shouldEqual
      List("1", "2", "3", "a", "b", "(+ c (- *))", "testing123")
  }

  it should "parse prefix operators" in {
    astOf("-b") shouldEqual "(- b)"
  }

  it should "parse infix operators" in {
    astOf("a - b") shouldEqual "(- a b)"
  }

  it should "parse postfix operators" in {
    astOf("x!") shouldEqual "(! x)"
  }

  it should "parse prefix and infix operators in the same expression" in {
    astOf("*a * *b") shouldEqual "(* (* a) (* b))"
  }

  it should "parse complex expressions with binary operators of different precedence" in {
    astOf("∀(n ∈ N : n^2 > n)") shouldEqual "(∀ (: (∈ n N) (> (^ n 2) n)))"
  }

  it should "parse lambdas" in {
    astOf("func (a, b, c) = a + b + c") shouldEqual "{a, b, c = (+ a (+ b c))}"
  }

  it should "parse function application" in {
    astOf("test(1, 2, 3 + 4)") shouldEqual "(test 1 2 (+ 3 4))"
  }

  it should "parse a complex expression" in {
    astOf("fn(1, func (a, b, c) = 3.14 * a + a * a)(1, 2, 3 + 4) + 5") shouldEqual
      "(+ ((fn 1 {a, b, c = (+ (* 3.14 a) (* a a))}) 1 2 (+ 3 4)) 5)"
  }
