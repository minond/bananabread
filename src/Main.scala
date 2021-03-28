package bisquit

import parser.{tokenize, parse, Syntax}


def main(args: Array[String]) =
  val syntax = Syntax.withUniop("-").withBinop("+").withBinop("-")
  println(tokenize("<stdin>", "  4.3 + 3 + a(1,2,345)"))
  println(parse("<stdin>", "  4.3", syntax))
  // println(parse("<stdin>", "  4.3 + 3 + a(1,2,345)", syntax))
