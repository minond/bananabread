package sourdough
package typechecker

import ir.{Ir, Typeless}
import ty.{Type, Var}


type Scope = Map[String, Type]

def infer(nodes: List[Typeless], scope: Scope): List[Type] =
  nodes.map(node => infer(node, scope))

def infer(node: Typeless, scope: Scope): Type = node match
  case Typeless.Num(num) => ty.I32
  case Typeless.Str(str) => ty.Str
  case Typeless.Id(id) => lookup(id.lexeme, scope)
  case Typeless.App(lambda, args, expr) => ???
  case Typeless.Lambda(params, body, expr) => ???

def lookup(label: String, scope: Scope): Type =
  scope.get(label) match
    case None => fresh()
    case Some(ty) => ty

val freshIds = LazyList.from(1).sliding(1)

def fresh() =
    Var(freshIds.next.head)
