package bananabread

import error.Err
import runtime.treewalker
import runtime.Interpreter
import printer.pp

import scala.util.{Try, Success, Failure}


def main(args: Array[String]) =
  val fileName = "./lib/Sample.bb"
  val code = loader.loadSource("Sample")

  val flagPrintOpcodes = args.contains("print-opcodes")
  val flagPrintState = args.contains("print-state")
  val flagDebug = args.contains("debug")

  val doDebug = flagDebug
  val doPrintOpcodes = flagPrintOpcodes || flagDebug
  val doPrintState = flagPrintState || flagDebug

  // val res = Try {
  //   for
  //     sources <- loader.load(fileName, code)
  //     ir <- program.lift(sources.head)
  //     ins <- backend.bytecode.compile(ir)
  //     _    = if doPrintOpcodes then
  //              println("~~~~~~~~~~~~~~~~~~~~~~~")
  //              println(pp(ins))
  //              println("~~~~~~~~~~~~~~~~~~~~~~~")
  //
  //     vm   = Interpreter(ins)
  //     _   <- if doDebug
  //            then vm.debugging.stepping.run
  //            else vm.run
  //   yield
  //     vm
  // }
  //
  // res match
  //   case Failure(ex: Exception) if ex.getMessage == "quit" =>
  //   case Failure(ex) => throw ex
  //   case Success(Left(err: Err)) => println(pp(err, code))
  //   case Success(Left(err)) => println(s"unhandled error: $err")
  //   case Success(Right(vm)) =>
  //     if doPrintState then
  //       println("~~~~~~~~~~~~~~~~~~~~~~~")
  //       vm.printState
  //       println("~~~~~~~~~~~~~~~~~~~~~~~")

  val res =
    for
      sources <- loader.load(fileName, code)
      nodes <- program.lift(sources.head)
      res = treewalker.run(nodes)
    yield
      res

  res match
    case Left(err: Err) => println(pp(err, code))
    case Left(err) => println(s"unhandled error: $err")
    case Right(walker) =>
      println(walker)
