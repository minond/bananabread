package bananabread

import parsing.language.{tokenize, parse}
import ir.typeless
import error.Errors
import error.pp => errpp
import runtime.Interpreter
import runtime.instruction.pp

import scala.io.Source


def main(args: Array[String]) =
  val fileName = "./lib/Sample.bb"
  val code = Source.fromFile(fileName).getLines.mkString("\n")

  println("~~~~~~~~~~~~~~~~~~~~~~~")
  val res =
    for
      ast <- parse(fileName, code)
      ir1 <- typeless.lift(ast)
      ir   = typeless.pass(ir1)
      ins <- backend.opcode.compile(ir)
      _    = println("~~~~~~~~~~~~~~~~~~~~~~~")
      _    = println(pp(ins))
      _    = println("~~~~~~~~~~~~~~~~~~~~~~~")
      _    = typechecker.infer(ir).map { tys =>
               ir.zip(tys._1).foreach {
                 case (typeless.Def(name, _, _), ty) => println(s"$name: $ty")
                 case (ir, ty) => println(s"$ir: $ty")
               }
               println("~~~~~~~~~~~~~~~~~~~~~~~")
             }
      vm   = Interpreter(ins)
      _   <- if args.headOption == Some("debug")
             then vm.debugging.stepping.run
             else vm.run
    yield
      vm

  res match
    case Right(vm) =>
      println("~~~~~~~~~~~~~~~~~~~~~~~")
      vm.printState
    case Left(err: Errors) => println(errpp(err, code))
    case Left(err) => println(s"unhandled error: $err")
