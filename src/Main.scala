package bisquit

import parser.{tokenize, parse, Syntax}


def main(args: Array[String]) =
  val syntax = Syntax.withUniop("-").withBinop("+").withBinop("-")
  println(tokenize("<stdin>", "  4.3 + 3 + a(1,2,345)", syntax))
  println(parse("<stdin>", "  4.3 + 54.764", syntax))
  println(parse("<stdin>", "b + c * 3", syntax))
  println(tokenize("<stdin>", "-3", syntax))
  println(parse("<stdin>", "-3", syntax))
  // println(parse("<stdin>", "  4.3 + 3 + a(1,2,345)", syntax))
