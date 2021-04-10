package sourdough
package test

import parser.{parse, Syntax}

val stdOps = Syntax().withPrefix(0, "-")
                     .withPrefix(0, "∀")
                     .withInfix(4, "^")
                     .withInfix(3, "*")
                     .withInfix(3, "/")
                     .withInfix(2, "+")
                     .withInfix(2, "-")
                     .withInfix(1, ":")
                     .withInfix(2, "∈")
                     .withInfix(1, ">")
                     .withPostfix(10, "!")

def exprsOf(code: String, syntax: Syntax = stdOps) =
  parse("<test>", code, syntax).getOrElse(???).map(_.toString)

def astOf(code: String, syntax: Syntax = stdOps) =
  exprsOf(code, syntax).head
