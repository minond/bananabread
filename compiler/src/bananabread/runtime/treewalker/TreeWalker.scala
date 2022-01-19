package bananabread
package runtime
package treewalker

import error._
import parsing.ast
import ir.stitched.Ir
import ir.stitched => ir


type Result = Either[InterpretationError, Scope]
type Scope = Map[String, Ir]


def run(nodes: List[Ir]): Result =
  nodes.foldLeft[Result](Right(Map.empty)) {
    case (err @ Left(_), _) =>
      return err
    case (Right(scope), node: ir.Def) =>
      handleDef(scope, node)
    case (Right(scope), node @ ir.App(id: ir.Id, _, _, _, _)) =>
      handleAppId(scope, node, id)
    case _ =>
      ???
  }


def handleDef(scope: Scope, node: ir.Def): Result =
  Right(scope + (node.name.lexeme -> node.value))

def handleAppId(scope: Scope, node: ir.App, id: ir.Id): Result =
  scope.get(id.expr.lexeme) match
    case None => Left(LookupErr(id.expr.lexeme, node))
    case Some(deref: ir.Id) => handleAppId(scope, node, deref)
    case Some(lambda: ir.Lambda) => handleAppLambda(scope, node, lambda)
    case Some(_) => /* bad app */ ???

def handleAppLambda(scope: Scope, node: ir.App, lambda: ir.Lambda): Result =
  println(lambda.params.map(_.name))
  Right(scope)
