package bananabread
package test

import org.scalatest._
import flatspec._
import matchers._

import runtime.value.I32


class VmTests extends AnyFlatSpec with should.Matchers:
  it should "evaluate add expressions" in {
    stackHeadOf("2 + 40") shouldEqual I32(42)
  }

  it should "evaluate nested add expressions" in {
    stackHeadOf("2 + 40 + 0 + 30") shouldEqual I32(72)
  }

  it should "evaluate nested sub expressions" in {
    stackHeadOf("2 - 40 - 0 - 30") shouldEqual I32(-68)
  }

  it should "evaluate deeply nested function calls" in {
    stackHeadOf(
      """
      let
        f = func () =
              func () =
                func () =
                  func () =
                    func (x) = x + x
      in
        f()()()()(4)
      """
    ) shouldEqual I32(8)
  }

  it should "evaluate recursive calls" in {
    stackHeadOf(
      """
      let
        count_down = func (x) =
          if x
          then count_down(x - 1)
          else x
      in
        count_down(10)
      """
    ) shouldEqual I32(0)
  }

  it should "evaluate indirect internal function calls" in {
    stackHeadOf(
      """
      let
        add_em = func (a, b) = a + b
        add_em_2 = add_em
      in
        add_em_2(3, 4)
      """
    ) shouldEqual I32(7)
  }

  it should "evaluate indirect global function calls" in {
    stackHeadOf(
      """
      def add_em(a, b) = a + b
      def add_em_2 = add_em

      add_em_2(7, 3)
      """
    ) shouldEqual I32(10)
  }
