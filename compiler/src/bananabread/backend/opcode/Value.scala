package bananabread
package backend.opcode

import error._
import ir.stitched
import ir.stitched.Ir
import runtime.value._
import utils.{safeToInt, squished}


type Lifted[T <: Value] = Either[GeneratorErr, T]


def lift(node: Ir): Lifted[Value] = node match
  case node: stitched.Num   => liftNum(node)
  case node: stitched.Lista => liftLista(node)

def liftNum(node: stitched.Num): Lifted[I32] =
  node.expr.lexeme.safeToInt match
    case Left(_)  => Left(BadNumErr(node))
    case Right(i) => Right(I32(i))

def liftLista(node: stitched.Lista): Lifted[Lista] =
  for
    liftedItems <- node.items.map(lift).squished
  yield
    Lista(liftedItems)
