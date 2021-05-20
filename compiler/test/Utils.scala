package bananabread
package test

import parsing.language.{Syntax, parse}
import ir.typeless
import runtime.Interpreter

val stdOps = Syntax.withPrefix(0, "-")
                   .withPrefix(0, "∀")
                   .withPrefix(0, "*")
                   .withPrefix(0, "opcode")
                   .withInfix(4, "^")
                   .withInfix(3, "*")
                   .withInfix(3, "/")
                   .withInfix(2, "+")
                   .withInfix(2, "-")
                   .withInfix(1, ":")
                   .withInfix(2, "∈")
                   .withInfix(1, ">")
                   .withPostfix(10, "!")

val prelude =
  """
  def +(a, b) =
    opcode %{
      load [I32] a
      load [I32] b
      add [I32]
    }

  def -(a, b) =
    opcode %{
      load [I32] a
      load [I32] b
      sub [I32]
    }

  def ++(a, b) =
    opcode %{
      load [Str] a
      load [Str] b
      concat
    }

  def println(x) =
    opcode %{
      load [Str] x
      println
    }
  """

def exprsOf(code: String, syntax: Syntax = stdOps) =
  parse("<test>", code, syntax).getOrElse(???).nodes.map(_.toString)

def astOf(code: String, syntax: Syntax = stdOps) =
  exprsOf(code, syntax).head

def resultOf(code: String, syntax: Syntax = stdOps) =
  val ast = parse("<stdin>", prelude + code, syntax).getOrElse(???)
  val ir = typeless.lift(ast)
  // import runtime.instruction.pp
  // backend.opcode.compile(ir) match
  //   case Right(xs) =>
  //     println("==========================")
  //     println(code)
  //     println("==========================")
  //     println(pp(xs))
  //     println("==========================")
  //   case Left(err) => println(err)
  val ins = backend.opcode.compile(ir).getOrElse(???)
  val interpreter = Interpreter(ins, false, false)
  interpreter.run
  interpreter

def stackHeadOf(code: String, syntax: Syntax = stdOps) =
  resultOf(code, syntax).stack.head
