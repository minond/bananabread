package bananabread
package runtime
package treewalker

import error._
import parsing.ast
import ir.stitched.Ir
import ir.stitched => ir
import utils.{Print, safeToInt}


sealed trait Value
case object Void extends Value with Print("void")
case class I32(value: Int) extends Value with Print(value.toString)


type Result[T] = Either[InterpretationError, T]
type Scoped[T] = Result[(Scope, T)]
type Scope = Map[String, Ir]


def run(nodes: List[Ir]): Result[Scope] =
  nodes.foldLeft[Result[Scope]](Right(Map.empty)) {
    case (err @ Left(_), _) =>
      return err
    case (Right(scope), node) =>
      run(scope, node).map(_._1)
  }
def run(scope: Scope, node: Ir): Scoped[Value] =
  node match
    case node: ir.Id =>
      handleId(scope, node)
    case node: ir.Num =>
      handleNum(scope, node)
    case node: ir.Def =>
      handleDef(scope, node)
    case node: ir.Opcode =>
      handleOpcode(scope, node)
  //   case (Right(scope), node @ ir.App(id: ir.Id, _, _, _, _)) =>
  //     handleAppId(scope, node, id)


def handleId(scope: Scope, node: ir.Id): Scoped[Value] =
  ???

def handleNum(scope: Scope, node: ir.Num): Scoped[Value] =
  node.expr.lexeme.safeToInt match
    case Left(_) => ???
    case Right(i) => Right((scope, I32(i)))

def handleDef(scope: Scope, node: ir.Def): Scoped[Value] =
  Right((scope + (node.name.lexeme -> node.value), Void))

def handleOpcode(scope: Scope, node: ir.Opcode): Scoped[Value] =
  Right((scope, Void))

// def handleAppId(scope: Scope, node: ir.App, id: ir.Id): Result =
//   scope.get(id.expr.lexeme) match
//     case None => Left(LookupErr(id.expr.lexeme, node))
//     case Some(deref: ir.Id) => handleAppId(scope, node, deref)
//     case Some(lambda: ir.Lambda) => handleAppLambda(scope, node, lambda)
//     case Some(_) => /* bad app */ ???
//
// def handleAppLambda(scope: Scope, node: ir.App, lambda: ir.Lambda): Result =
//   ???
//   // println(lambda.params.map(_.name))
//   // Right(scope)
