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
  val prelude = module.loadSource("Prelude")
  val sample = module.loadSource("Sample")
  val code = prelude + sample

  val flagPrintOpcodes = args.contains("print-opcodes")
  val flagPrintTypes = args.contains("print-types")
  val flagPrintState = args.contains("print-state")
  val flagDebug = args.contains("debug")

  val doDebug = flagDebug
  val doPrintOpcodes = flagPrintOpcodes || flagDebug
  val doPrintTypes = flagPrintTypes || flagDebug
  val doPrintState = flagPrintState || flagDebug

  val res =
    for
      ast <- parse(fileName, code)
      ir1 <- typeless.lift(ast)
      ir   = typeless.pass(ir1)
      ins <- backend.opcode.compile(ir)

      _    = if doPrintOpcodes then
               println("~~~~~~~~~~~~~~~~~~~~~~~")
               println(pp(ins))
               println("~~~~~~~~~~~~~~~~~~~~~~~")

      _    = if doPrintTypes then
               typechecker.infer(ir).map { tys =>
                 println("~~~~~~~~~~~~~~~~~~~~~~~")
                 ir.zip(tys._1).foreach {
                   case (typeless.Def(name, _, _), ty) => println(s"$name: $ty")
                   case (ir, ty) => println(s"$ir: $ty")
                 }
                 println("~~~~~~~~~~~~~~~~~~~~~~~")
               }

      vm   = Interpreter(ins)

      _   <- if doDebug
             then vm.debugging.stepping.run
             else vm.run
    yield
      vm

  res match
    case Right(vm) =>
      if doPrintState then
        println("~~~~~~~~~~~~~~~~~~~~~~~")
        vm.printState
        println("~~~~~~~~~~~~~~~~~~~~~~~")
    case Left(err: Errors) => println(errpp(err, code))
    case Left(err) => println(s"unhandled error: $err")
