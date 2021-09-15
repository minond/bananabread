package bananabread
package test

import org.scalatest._
import flatspec._
import matchers._

import parsing.Syntax
import parsing.language.parse


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
    astOf("∀(n ∈ N -> n^2 > n)") shouldEqual "(∀ (-> (∈ n N) (> (^ n 2) n)))"
  }

  it should "parse lambdas" in {
    astOf("func (a, b, c) = a + b + c") shouldEqual "{a, b, c = (+ (+ a b) c)}"
  }

  it should "parse lambdas with type tags" in {
    astOf("func (a : Str, b : I32, c : Symbol) : Str = a + b + c") shouldEqual "{a : Str, b : I32, c : Symbol = (+ (+ a b) c) : Str}"
  }

  it should "parse function application" in {
    astOf("test(1, 2, 3 + 4)") shouldEqual "(test 1 2 (+ 3 4))"
  }

  it should "parse a complex expression" in {
    astOf("fn(1, func (a, b, c) = 3.14 * a + a * a)(1, 2, 3 + 4) + 5") shouldEqual
      "(+ ((fn 1 {a, b, c = (+ (* 3.14 a) (* a a))}) 1 2 (+ 3 4)) 5)"
  }

  it should "correctly order operator with equal precedence (1)" in {
    astOf("2 - 1 + 5") shouldEqual "(+ (- 2 1) 5)"
  }

  it should "correctly order operator with equal precedence (2)" in {
    astOf("1 + 2 / 1 * 5") shouldEqual "(+ 1 (* (/ 2 1) 5))"
  }

  // TODO This is broken
  // it should "correctly order operator with equal precedence (3)" in {
  //   astOf("1 + 2 / 1 * 5 + 7") shouldEqual "(+ (+ 1 (* (/ 2 1) 5)) 7)"
  // }

  it should "correctly order operator with higher precedence" in {
    astOf("2 + 1 * 5") shouldEqual "(+ 2 (* 1 5))"
  }

  it should "correctly order operator with lower precedence" in {
    astOf("2 * 1 - 5") shouldEqual "(- (* 2 1) 5)"
  }

  it should "correctly order operators in a complex equation" in {
    astOf("3 + 1 / 4 * 1 + 9") shouldEqual "(+ (+ 3 (/ 1 (* 4 1))) 9)"
  }

  it should "parse quoted strings" in {
    astOf(""""hi there"""") shouldEqual """"hi there""""
  }

  it should "parse block strings" in {
    astOf("""%{hi there}""") shouldEqual """"hi there""""
  }
