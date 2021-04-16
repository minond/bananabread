package sourdough
package typechecker

import ir.{Typeless => tl}
import ir.Ir
import ty.{Type, Var}


type Scope = Map[String, Type]

def infer(nodes: List[Ir], scope: Scope): List[Type] =
  nodes.map(node => infer(node, scope))

def infer(node: Ir, scope: Scope): Type = node match
  case tl.Num(num) => ty.I32
  case tl.Str(str) => ty.Str
  case tl.Id(id) => lookup(id.lexeme, scope)
  case tl.App(lambda, args, expr) => ???
  case tl.Lambda(params, body, expr) => ???

def lookup(label: String, scope: Scope): Type =
  scope.get(label) match
    case None => fresh()
    case Some(ty) => ty

val freshIds = LazyList.from(1).sliding(1)

def fresh() =
    Var(freshIds.next.head)
