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

  it should "evaluate a mix of indirect internal and global function calls" in {
    stackHeadOf(
      """
      def global_add_em(a, b) = a + b
      def global_add_em_2 = global_add_em

      let
        internal_add_em = func (a, b) = a + b
        internal_add_em_2 = internal_add_em
      in
        global_add_em_2(internal_add_em_2(3, 4), 7)
      """
    ) shouldEqual I32(14)
  }

  it should "resolve variable scopes" in {
    stackHeadOf(
      """
      let
        a = 1
      in
        let
          b = 2
        in
          a + b
      """
    ) shouldEqual I32(3)
  }

  // TODO: Wrapping code in a begin/end block because it wasn't being properly
  // parsed outside of one after the prelude started being prepended to the
  // test code. Fix this since it's a general problem with parsing definitions
  // plus top-level code in source.
  it should "evaluate immediatelly invoked functions" in {
    stackHeadOf(
      """
      begin
        (func (xxx) = xxx + xxx)(3)
      end
      """
    ) shouldEqual I32(6)
  }

  it should "use lexical scope when evaluating functions" in {
    stackHeadOf(
      """
      let
        base = 1

        start =
          func (a) =
            func (b) =
              func (c) =
                a + b + c + base

        a = start(2)
        a_b = a(5)
        a_b_c = a_b(7) // 2 + 5 + 7 + 1 = 15

        ab = start(2)(3)
        ab_c = ab(6) // 2 + 5 + 6 + 1 = 12

        abc = start(2)(3)(4) // 2 + 3 + 4 + 1 = 10
      in
        a_b_c + ab_c + abc // 15 + 12 + 10 = 37
      """
    ) shouldEqual I32(37)
  }

  it should "use lexical scope when evaluating global functions" in {
    stackHeadOf(
      """
      def base = 10

      def start =
        func (a) =
          func (b) =
            func (c) =
              a + b + c + base

      def a = start(2)
      def a_b = a(5)
      def a_b_c = a_b(7) // 2 + 5 + 7 + 10 = 24

      def ab = start(2)(3)
      def ab_c = ab(6) // 2 + 5 + 6 + 10 = 21

      def abc = start(2)(3)(4) // 2 + 3 + 4 + 10 = 19

      def res = a_b_c + ab_c + abc // 24 + 21 + 19 = 64

      res
      """
    ) shouldEqual I32(64)
  }

  it should "handle user opcode jumps" in {
    stackHeadOf(
      """
      def x = 123

      def jump_test_1() =
        opcode %{
          jmp jump_test_2_entry
        }

      def jump_test_2() =
        opcode %{
          halt
        jump_test_2_entry:
          load [Ref] x
        }

      jump_test_1()
      """
    ) shouldEqual I32(123)
  }
