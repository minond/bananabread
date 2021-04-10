package sourdough

import parser.{tokenize, parse, Syntax}

def main(args: Array[String]) =
  val syntax = Syntax.withPrefix(1, "-")
                     .withPrefix(1, "*")
                     .withPrefix(1, "∀")
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
                     .withPostfix(1, "!")
                     .withPostfix(1, "++")

  println(parse("<stdin>", "  4.3 + 54.764", syntax))
  println(parse("<stdin>", "b + c * 3", syntax))
  println(parse("<stdin>", "a - a", syntax))
  println(parse("<stdin>", "a - -a", syntax))
  println(parse("<stdin>", "-3", syntax))
  println(parse("<stdin>", "_1+_2", syntax))
  println(parse("<stdin>", "_1+_2@3", syntax))
  println(parse("<stdin>", "--_++", syntax))
  println(parse("<stdin>", "1 + 2 * 3", syntax))
  println(parse("<stdin>", "1 * 2 + 3!", syntax))
  println(parse("<stdin>", "a()()()()() + 1", syntax))
  println(parse("<stdin>", "  4.3 + 3 + -a(1,2,345)!", syntax))
  println(parse("<stdin>", "abc(1, 2, 3)!", syntax))
  println(parse("<stdin>", "(1 + 2) * 3", syntax))
  println(parse("<stdin>", "1 + 2 * 3", syntax))
  println(parse("<stdin>", "1 + (2 * 3)", syntax))
  println(parse("<stdin>", "genfunc(a, b)(c, d, 1 + 2) + (2!)!", syntax))
  println(parse("<stdin>", "fn(1, func (a, b, c) = 3.14 * a + a * a)(1, 2, 3 + 4) + 5", syntax))
  println(parse("<stdin>", "*a * *b", syntax))
  println(parse("<stdin>", "∀ n ∈ N : n^2 > n", syntax))
  println(parse("<stdin>", "fn(1, func (a, b, c) = 3.14 * a + a * a)(1, 2, 3 + 4) + 5", syntax).map(ir.Typeless.lift))
  println(parse("<stdin>", "∀ n ∈ N : n^2 > n", syntax).map(ir.Typeless.lift))
  println(parse("<stdin>", "+(1, *(2, 3))", syntax).map(ir.Typeless.lift)) // TODO: Left(UnexpectedTokenErr(COMMA))
