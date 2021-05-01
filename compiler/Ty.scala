package bananabread
package ty

import ir.Typeless => tl
import ir.Typeless.Ir


sealed trait Type
case object I32 extends Type
case object Str extends Type
case class Var(id: Int) extends Type
case class Lambda(in: List[Type], out: List[Type]) extends Type


type Scope = Map[String, Type]


def infer(nodes: List[Ir], scope: Scope): List[Type] =
  nodes.map(node => infer(node, scope))

def infer(node: Ir, scope: Scope): Type = node match
  case tl.Num(num) => ty.I32
  case tl.Str(str) => ty.Str
  case tl.Id(id) => lookup(id.lexeme, scope)
  case tl.App(lambda, args, expr) => ???
  case tl.Lambda(params, body, expr) => ???
  case _: tl.Cond => ???
  case _: tl.Let => ???
  case _: tl.Begin => ???

def lookup(label: String, scope: Scope): Type =
  scope.get(label) match
    case None => fresh()
    case Some(ty) => ty

val freshIds = LazyList.from(1).sliding(1)

def fresh() =
    Var(freshIds.next.head)
