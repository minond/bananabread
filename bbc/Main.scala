package bananabread

import parser.{tokenize, parse, Syntax}
import ir.Typeless => tl
import opcode.{Opcode, Instruction}
import vm.Machine

def main(args: Array[String]) =
  val syntax = Syntax.withPrefix(1, "-")
                     .withPrefix(1, "*")
                     .withPrefix(1, "∀")
                     .withPrefix(1, "!")
                     .withInfix(4, "^")
                     .withInfix(3, "*")
                     .withInfix(3, "/")
                     .withInfix(3, "%")
                     .withInfix(2, "+")
                     .withInfix(2, "-")
                     .withInfix(0, "|>")
                     .withInfix(0, "_o_")
                     .withInfix(10, "∈")
                     .withInfix(99, "^")
                     .withInfix(2, ":")
                     .withInfix(3, ">")
                     .withInfix(3, "==")
                     .withInfix(0, ":=")
                     .withInfix(0, "++")
                     .withPostfix(1, "!")

  // println(parse("<stdin>", "  4.3 + 54.764", syntax))
  // println(parse("<stdin>", "b + c * 3", syntax))
  // println(parse("<stdin>", "a - a", syntax))
  // println(parse("<stdin>", "a - -a", syntax))
  // println(parse("<stdin>", "-3", syntax))
  // println(parse("<stdin>", "_1+_2", syntax))
  // println(parse("<stdin>", "_1+_2@3", syntax))
  // println(parse("<stdin>", "--_++", syntax))
  // println(parse("<stdin>", "1 + 2 * 3", syntax))
  // println(parse("<stdin>", "1 * 2 + 3!", syntax))
  // println(parse("<stdin>", "a()()()()() + 1", syntax))
  // println(parse("<stdin>", "  4.3 + 3 + -a(1,2,345)!", syntax))
  // println(parse("<stdin>", "abc(1, 2, 3)!", syntax))
  // println(parse("<stdin>", "(1 + 2) * 3", syntax))
  // println(parse("<stdin>", "1 + 2 * 3", syntax))
  // println(parse("<stdin>", "1 + (2 * 3)", syntax))
  // println(parse("<stdin>", "genfunc(a, b)(c, d, 1 + 2) + (2!)!", syntax))
  // println(parse("<stdin>", "fn(1, func (a, b, c) = 3.14 * a + a * a)(1, 2, 3 + 4) + 5", syntax))
  // println(parse("<stdin>", "*a * *b", syntax))
  // println(parse("<stdin>", "∀ n ∈ N : n^2 > n", syntax))
  // println(parse("<stdin>", "fn(1, func (a, b, c) = 3.14 * a + a * a)(1, 2, 3 + 4) + 5", syntax).map(ir.Typeless.lift))
  // println(parse("<stdin>", "∀ n ∈ N : n^2 > n", syntax).map(ir.Typeless.lift))
  // println(parse("<stdin>", "+(1, *(2, 3))", syntax).map(ir.Typeless.lift)) // TODO: Left(UnexpectedTokenErr(COMMA))
  // println(parse("<stdin>", "if a == b then b() else if a == c then c() else a()", syntax).map(ir.Typeless.lift))
  // println(parse("<stdin>", "let x = 1 y = x + 2 in x + y", syntax).map(ir.Typeless.lift))
  // println(parse("<stdin>", "1", syntax).map(ir.Typeless.lift).map(node => typechecker.infer(node, Map.empty)))
  // println(parse("<stdin>", "let x = ref(0) in x := !x + 1", syntax).map(ir.Typeless.lift))
  // println(parse("<stdin>", "1 + 2", syntax).map(ir.Typeless.lift).map(opcode.lift).getOrElse(???).toSeq :+ Instruction(opcode.Halt))
  // println(parse("<stdin>", "1", syntax).map(ir.Typeless.lift).getOrElse(???).head)

  val instructions = parse("<stdin>",
    """
    let
      ++ = func (a, b) = a + b
      cond = func (a, b) = a ++ b
    in
      if cond(0, 1)
      then 123
      else 1
    """, syntax).map(tl.lift).map(opcode.compile).getOrElse(???).dump

  // val instructions = parse("<stdin>",
  //   """
  //   let
  //     a = 1+3
  //     b = func (x) = x
  //     c = func (x) = x+x
  //     d = if 0
  //         then 123
  //         else a
  //     e = func (x) =
  //           if x
  //           then x
  //           else x + 1
  //     f = func () =
  //           func () =
  //             func () =
  //               func () =
  //                 func (x) = x + x
  //   in f()()()()(4)
  //   """, syntax).map(tl.lift).map(opcode.compile).getOrElse(???).dump

  // val instructions = parse("<stdin>",
  //   """
  //       (func () = func () = func (a, b) = a + b)()()(4, 3)
  //   """, syntax).map(tl.lift).map(opcode.compile).getOrElse(???).dump
  //       // (func (a) = func (b) = a + b)(4)(3)

  val rt = Machine(instructions)

  rt.printInstructions

  while rt.running do rt.next
  rt.printInfo
