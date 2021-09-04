package bananabread
package test

import parsing.language.{Syntax, parse}
import ir.typeless
import runtime.Interpreter
import error.Errors
import error.pp => errpp

val stdOps = Syntax.withPrefix(0, "-")
                   .withPrefix(0, "∀")
                   .withPrefix(0, "*")
                   .withPrefix(0, "opcode")
                   .withInfix(4, "^")
                   .withInfix(3, "*")
                   .withInfix(3, "/")
                   .withInfix(2, "+")
                   .withInfix(2, "-")
                   .withInfix(0, "->")
                   .withInfix(2, "∈")
                   .withInfix(1, ">")
                   .withPostfix(10, "!")

def exprsOf(code: String, syntax: Syntax = stdOps) =
  parse("<test>", code, syntax) match
    case Left(err) => throw Exception(errpp(err, code))
    case Right(expr) => expr.nodes.map(_.toString)

def astOf(code: String, syntax: Syntax = stdOps) =
  exprsOf(code, syntax).head

def resultOf(code: String, syntax: Syntax = stdOps) =
  val prelude = module.loadSource("Prelude")
  val interpreter =
    for
      ast <- parse("<stdin>", prelude + code, syntax)
      ir1 <- typeless.lift(ast)
      ir   = typeless.pass(ir1)
      ins <- backend.opcode.compile(ir)
      interpreter = Interpreter(ins, false, false)
    yield interpreter

  interpreter match
    case Left(err: Errors) =>
      println(errpp(err, prelude + code))
      ???
    case Left(err) =>
      println(s"unhandled error: $err")
      ???
    case Right(vm) =>
      vm.run
      vm

def stackHeadOf(code: String, syntax: Syntax = stdOps) =
  val ret = resultOf(code, syntax)
  ret.stack.get(ret.registers.esp.value - 1)
