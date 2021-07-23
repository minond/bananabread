package bananabread
package typechecker

import error._
import ty._
import ir.typeless
import ir.typeless.Ir

import utils.squished


type Scope = Map[String, Type]
type Inferred[T] = Either[InferenceErr, T]


def infer(nodes: List[Ir], scope: Scope = Map.empty): Inferred[List[Type]] =
  nodes.map(node => infer(node, scope)).squished
def infer(node: Ir, scope: Scope): Inferred[Type] = node match
  case _: typeless.Num     => Right(ty.I32)
  case _: typeless.Str     => Right(ty.Str)
  case _: typeless.Symbol  => Right(ty.Symbol)
  case ir: typeless.Id     => inferId(ir, scope)
  case ir: typeless.Begin  => inferBegin(ir, scope)
  case ir: typeless.Def    => inferDef(ir, scope)
  case ir: typeless.App    => inferApp(ir, scope)
  case ir: typeless.Lambda => inferLambda(ir, scope)
  case ir: typeless.Cond   => inferCond(ir, scope)
  case ir: typeless.Let    => inferLet(ir, scope)

def inferId(id: typeless.Id, scope: Scope): Inferred[Type] =
  lookup(id, id.id.lexeme, scope)

def inferBegin(begin: typeless.Begin, scope: Scope): Inferred[Type] =
  infer(begin.ins.last, scope)

def inferDef(defIr: typeless.Def, scope: Scope): Inferred[Type] =
  infer(defIr.value, scope)

def inferApp(app: typeless.App, scope: Scope): Inferred[Type] =
  ???

def inferLambda(lam: typeless.Lambda, scope: Scope): Inferred[Lambda] =
  ???

def inferCond(cond: typeless.Cond, scope: Scope): Inferred[Type] =
  ???

def inferLet(let: typeless.Let, scope: Scope): Inferred[Type] =
  ???


def lookup(ir: Ir, label: String, scope: Scope): Either[LookupErr, Type] =
  scope.get(label) match
    case None => Left(LookupErr(label, ir))
    case Some(ty) => Right(ty)

val ids = LazyList.from(1).sliding(1)
def fresh() =
  Var(ids.next.head)
