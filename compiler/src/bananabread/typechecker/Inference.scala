package bananabread
package typechecker

import error._
import ty._
import ir.typeless
import ir.typeless.Ir
import parsing.ast

import utils.squished


type Scope = Map[String, Type]
type Inferred[T] = Either[InferenceErr, T]


def infer(nodes: List[Ir], scope: Scope = Map.empty): Inferred[List[Type]] =
  nodes.map(node => infer(node, scope)).squished
def infer(node: Ir, scope: Scope): Inferred[Type] = node match
  case _: typeless.Num     => Right(ty.I32)
  case _: typeless.Str     => Right(ty.Str)
  case _: typeless.Symbol  => Right(ty.Symbol)
  case _: typeless.Bool    => Right(ty.Bool)
  case ir: typeless.Id     => inferId(ir, scope)
  case ir: typeless.Begin  => inferBegin(ir, scope)
  case ir: typeless.Def    => inferDef(ir, scope)
  case ir: typeless.App    => inferApp(ir, scope)
  case ir: typeless.Lambda => inferLambda(ir, scope)
  case ir: typeless.Cond   => inferCond(ir, scope)
  case ir: typeless.Let    => inferLet(ir, scope)

def inferId(id: typeless.Id, scope: Scope): Inferred[Type] =
  lookup(id, id.expr.lexeme, scope)

def inferBegin(begin: typeless.Begin, scope: Scope): Inferred[Type] =
  infer(begin.ins.last, scope)

def inferDef(defIr: typeless.Def, scope: Scope): Inferred[Type] =
  infer(defIr.value, scope)

def inferApp(app: typeless.App, scope: Scope): Inferred[Type] =
  ???

// TODO Handle type variables
def inferLambda(lam: typeless.Lambda, scope: Scope): Inferred[Lambda] =
  for
    paramTys <- inferLambdaParams(lam, scope)
    retTys   <- inferLambdaRets(lam, scope)
  yield
    Lambda(paramTys, retTys)

// TODO Infer untagged parameters
def inferLambdaParams(lam: typeless.Lambda, scope: Scope): Inferred[List[Type]] =
  val paramTags = lam.expr.params.map(_.ty)
  val paramTys = lam.params.zip(paramTags).map {
    case (param, Some(tag)) =>
      parseType(param, tag) match
        case Right(ty) => ty
        case Left(err) => return Left(err)

    case (param, _) =>
      ???
  }

  Right(paramTys)

// TODO Infer untagged return type
def inferLambdaRets(lam: typeless.Lambda, scope: Scope): Inferred[List[Type]] =
  val retTag = lam.expr.tyRet
  val retTy =
    retTag match
      case Some(tag) =>
        parseType(lam, tag) match
          case Right(ty) => ty
          case Left(err) => return Left(err)

      case None =>
        ???

  Right(List(retTy))

def inferCond(cond: typeless.Cond, scope: Scope): Inferred[Type] =
  for
    condTy <- infer(cond.cond, scope)
    _      <- condTy.ensure(Bool, cond.cond)
    passTy <- infer(cond.pass, scope)
    failTy <- infer(cond.fail, scope)
    _      <- failTy.ensure(passTy, cond.fail)
  yield
    passTy

def inferLet(let: typeless.Let, scope: Scope): Inferred[Type] =
  val subscope = let.bindings.foldLeft(scope) { (scope, binding) =>
    infer(binding.value, scope) match
      case Right(ty) => scope + (binding.label.lexeme -> ty)
      case Left(err) => return Left(err)
  }

  infer(let.body, subscope)


def lookup(ir: Ir, label: String, scope: Scope): Either[LookupErr, Type] =
  scope.get(label) match
    case None => Left(LookupErr(label, ir))
    case Some(ty) => Right(ty)

val ids = LazyList.from(1).sliding(1)
def fresh() =
  Var(ids.next.head)


def parseType(ir: Ir, node: ast.Ty): Either[UnknowTypeErr, Type] =
  node.ty.lexeme match
    case "I32"    => Right(ty.I32)
    case "Str"    => Right(ty.Str)
    case "Symbol" => Right(ty.Symbol)
    case "Bool"   => Right(ty.Bool)
    case baddie   => Left(UnknowTypeErr(baddie, ir))


extension (ty: Type)
  def ensure(expected: Type, node: Ir): Either[TypeMismatchErr, Type] =
    if ty == expected
    then Right(ty)
    else Left(TypeMismatchErr(expected, ty, node))
