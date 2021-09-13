package bananabread

import parsing.language.{tokenize, parse}
import ir.typeless
import error.Errors
import error.pp => errpp
import runtime.Interpreter
import runtime.instruction.pp

import scala.io.Source
import scala.util.{Try, Success, Failure}


def main(args: Array[String]) =
  val fileName = "./lib/Sample.bb"
  val prelude = module.loadSource("Prelude")
  val sample = module.loadSource("Sample")
  val code = prelude + sample

  val flagPrintAst = args.contains("print-ast")
  val flagPrintOpcodes = args.contains("print-opcodes")
  val flagPrintTypes = args.contains("print-types")
  val flagPrintState = args.contains("print-state")
  val flagDebug = args.contains("debug")

  val doDebug = flagDebug
  val doPrintAst = flagPrintAst || flagDebug
  val doPrintOpcodes = flagPrintOpcodes || flagDebug
  val doPrintTypes = flagPrintTypes || flagDebug
  val doPrintState = flagPrintState || flagDebug

  val res =
    for
      ast <- parse(fileName, code)
      _    = if doPrintAst then
               println("~~~~~~~~~~~~~~~~~~~~~~~")
               println(ast)
               println("~~~~~~~~~~~~~~~~~~~~~~~")

      ir1 <- typeless.lift(ast)
      ir   = typeless.pass(ir1)
      tys <- typechecker.infer(ir)
      _    = if doPrintTypes then
               println("~~~~~~~~~~~~~~~~~~~~~~~")
               ir.zip(tys._1).foreach {
                 case (typeless.Def(name, _, _), ty) => println(s"$name: $ty")
                 case (ir, ty) => println(s"$ir: $ty")
               }
               println("~~~~~~~~~~~~~~~~~~~~~~~")

      ins <- backend.opcode.compile(ir)
      _    = if doPrintOpcodes then
               println("~~~~~~~~~~~~~~~~~~~~~~~")
               println(pp(ins))
               println("~~~~~~~~~~~~~~~~~~~~~~~")

      vm   = Interpreter(ins)
    yield
      if doDebug then
        Try { vm.debugging.stepping.run } match
          case Success(_)  =>
          case Failure(ex: Exception) if ex.getMessage == "quit" =>
          case Failure(ex) =>
            throw ex
      else
        vm.run

      vm

  res match
    case Right(vm) =>
      if doPrintState then
        println("~~~~~~~~~~~~~~~~~~~~~~~")
        vm.printState
        println("~~~~~~~~~~~~~~~~~~~~~~~")
    case Left(err: Errors) => println(errpp(err, code))
    case Left(err) => println(s"unhandled error: $err")
