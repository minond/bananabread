package sourdough

import parser.{tokenize, parse, Syntax}
import ir.Typeless => tl
import opcode.Opcode
import runtime.Instruction
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
                     .withPostfix(1, "!")
                     .withPostfix(1, "++")

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
  // println(parse("<stdin>", "1 + 2", syntax).map(ir.Typeless.lift).map(runtime.lift).getOrElse(???).toSeq :+ Instruction(opcode.Halt))
  // println(parse("<stdin>", "1", syntax).map(ir.Typeless.lift).getOrElse(???).head)

  val instructions = parse("<stdin>",
    """
    let cond = 777 in
      if cond
      then 123
      else 321
    """, syntax).map(tl.lift).map(runtime.lift).getOrElse(???).toSeq :+ Instruction(opcode.Halt)

  val rt = Machine(instructions)

  println(instructions.mkString("\n"))

  rt.next
  rt.next
  rt.next
  rt.next
  rt.next
  rt.next
  rt.next
  rt.next
  rt.next
  rt.next
  rt.next
  rt.next
  rt.next
  rt.next
  rt.next
  rt.next
  println(rt.stack.pop)
