package bisquit

import parser.parse


def main(args: Array[String]) =
  println(parse("<repl>", "  4.3 ").toList)
