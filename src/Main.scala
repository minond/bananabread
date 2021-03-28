package bisquit

import parser.{parse, SyntaxExtension}


def main(args: Array[String]) =
  val ext = SyntaxExtension.withUniop("-").withBinop("+").withBinop("-")
  println(parse("<stdin>", "  4.3 + 3 a(1,2,345)", ext).toList)
