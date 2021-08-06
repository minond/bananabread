package bananabread

import parsing.language.{tokenize, parse}
import ir.typeless
import error.Errors
import error.pp => errpp
import runtime.Interpreter
import runtime.instruction.pp

import scala.io.Source


def main(args: Array[String]) =
  val code = Source.fromFile("./lib/Sample.bb").getLines.mkString

  println("~~~~~~~~~~~~~~~~~~~~~~~")
  val res =
    for
      ast <- parse("<stdin>", code)
      ir  <- typeless.lift(ast)
      tys <- typechecker.infer(ir)
      ins <- backend.opcode.compile(ir)
      _   <- Interpreter(ins).run //.debugging//.stepping
    yield
      println("~~~~~~~~~~~~~~~~~~~~~~~")
      println(pp(ins))
      println("~~~~~~~~~~~~~~~~~~~~~~~")
      ir.zip(tys._1).foreach {
        case (typeless.Def(name, _, _), ty) => println(s"$name: $ty")
        case (ir, ty) => println(s"$ir: $ty")
      }
      println("~~~~~~~~~~~~~~~~~~~~~~~")

      "ok"

  res match
    case Right(msg) => println(msg)
    case Left(err: Errors) => println(errpp(err, code))
    case Left(err) => println(s"unhandled error: $err")
