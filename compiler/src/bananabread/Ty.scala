package bananabread
package ty

import ir.typeless
import ir.typeless.Ir


sealed trait Type
case object I32 extends Type
case object Str extends Type
case object Symbol extends Type
case class Var(id: Int) extends Type
case class Lambda(in: List[Type], out: List[Type]) extends Type


type Scope = Map[String, Type]


def infer(nodes: List[Ir], scope: Scope): List[Type] =
  nodes.map(node => infer(node, scope))

def infer(node: Ir, scope: Scope): Type = node match
  case typeless.Num(_) => ty.I32
  case typeless.Str(_) => ty.Str
  case typeless.Symbol(_) => ty.Symbol
  case typeless.Id(id) => lookup(id.lexeme, scope)
  case typeless.App(lambda, args, expr) => ???
  case typeless.Lambda(params, body, expr) => ???
  case _: typeless.Cond => ???
  case _: typeless.Let => ???
  case _: typeless.Begin => ???
  case typeless.Def(_, value, _) => infer(value, scope)

def lookup(label: String, scope: Scope): Type =
  scope.get(label) match
    case None => fresh()
    case Some(ty) => ty

val freshIds = LazyList.from(1).sliding(1)

def fresh() =
  Var(freshIds.next.head)
