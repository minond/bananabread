package bananabread
package typechecker

import error._
import ty._
import ir.typeless
import ir.typeless.Ir
import parsing.ast

import utils.squished

import scala.reflect.ClassTag


type Scope = Map[String, Type]
type Scoped[T] = Inferred[(T, Scope)]
type Inferred[T] = Either[InferenceErr, T]


def infer(nodes: List[Ir], scope: Scope = Map.empty): Scoped[List[Type]] =
  nodes.foldLeft[Scoped[List[Type]]](Right((List.empty, scope))) {
    case (Left(err), _) =>
      Left(err)
    case (Right((tys, scope)), node) =>
      infer(node, scope) match
        case Left(err) => Left(err)
        case Right((ty, next)) =>
          Right((tys :+ ty, next))
  }
def infer(node: Ir, scope: Scope): Scoped[Type] = node match
  case _: typeless.Num     => Right((ty.I32, scope))
  case _: typeless.Str     => Right((ty.Str, scope))
  case _: typeless.Symbol  => Right((ty.Symbol, scope))
  case _: typeless.Bool    => Right((ty.Bool, scope))
  case ir: typeless.Id     => inferId(ir, scope)
  case ir: typeless.Begin  => inferBegin(ir, scope)
  case ir: typeless.Def    => inferDef(ir, scope)
  case ir: typeless.App    => inferApp(ir, scope)
  case ir: typeless.Lambda => inferLambda(ir, scope)
  case ir: typeless.Cond   => inferCond(ir, scope)
  case ir: typeless.Let    => inferLet(ir, scope)

def inferId(id: typeless.Id, scope: Scope): Scoped[Type] =
  lookup(id, id.expr.lexeme, scope).flatMap { ty => Right((ty, scope)) }

def inferBegin(begin: typeless.Begin, scope: Scope): Scoped[Type] =
  infer(begin.ins.last, scope)

// TODO This needs to return a new and updated scope
def inferDef(defIr: typeless.Def, scope: Scope): Scoped[Type] =
  infer(defIr.value, scope)

// TODO Apply argTys
def inferApp(app: typeless.App, scope: Scope): Scoped[Type] =
  for
    argTys <- app.args.map { arg => infer(arg, scope) }.squished
    called <- infer(app.lambda, scope)
    fnTy   <- called._1.expect[Lambda](app.lambda)
  yield
    (fnTy, scope)

// TODO Handle type variables
def inferLambda(lam: typeless.Lambda, scope: Scope): Scoped[Lambda] =
  for
    paramTys <- inferLambdaParams(lam, scope)
    retTys   <- inferLambdaRets(paramTys, lam, scope)
  yield
    (Lambda(paramTys, retTys), scope)

def inferLambdaParams(lam: typeless.Lambda, scope: Scope): Inferred[List[Type]] =
  val tags = lam.expr.params.map(_.ty)
  val params = lam.params

  params.zip(tags).foldLeft[Inferred[List[Type]]](Right(List.empty)) {
    case (Right(acc), (param, None)) =>
      Right(acc :+ fresh())

    case (Right(acc), (param, Some(tag))) =>
      parseType(tag) match
        case Right(ty) => Right(acc :+ ty)
        case Left(err) => Left(err)

    case (Left(err), _) =>
      Left(err)
  }

// TODO Substitute type variables
def inferLambdaRets(paramTys: List[Type], lam: typeless.Lambda, scope: Scope): Inferred[List[Type]] =
  lam.expr.tyRet match
    case Some(tag) =>
      parseType(tag) match
        case Right(ty) => Right(List(ty))
        case Left(err) => Left(err)

    case None =>
      val lexicalScope = lam.expr.params
        .map { param => param.name.lexeme }
        .zip(paramTys)
        .foldLeft(scope) { case (scope, (name, ty)) => scope + (name -> ty) }

      for
        ty <- infer(lam.body, lexicalScope)
      yield
        List(ty._1)

def inferCond(cond: typeless.Cond, scope: Scope): Scoped[Type] =
  for
    condTy <- infer(cond.cond, scope)
    _      <- condTy._1.ensure(Bool, cond.cond)
    passTy <- infer(cond.pass, scope)
    failTy <- infer(cond.fail, scope)
    _      <- failTy._1.ensure(passTy._1, cond.fail)
  yield
    (passTy._1, scope)

def inferLet(let: typeless.Let, scope: Scope): Scoped[Type] =
  val subscope = let.bindings.foldLeft(scope) { (scope, binding) =>
    infer(binding.value, scope) match
      case Right((ty, _)) => scope + (binding.label.lexeme -> ty)
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


def parseType(node: ast.Ty): Either[UnknowTypeErr, Type] =
  node.ty.lexeme match
    case "I32"    => Right(ty.I32)
    case "Str"    => Right(ty.Str)
    case "Symbol" => Right(ty.Symbol)
    case "Bool"   => Right(ty.Bool)
    case baddie   => Left(UnknowTypeErr(node))


extension (ty: Type)
  def ensure(expected: Type, node: Ir): Either[TypeMismatchErr, Type] =
    if ty == expected
    then Right(ty)
    else Left(TypeMismatchErr(expected, ty, node))

  def expect[T <: Type : ClassTag](node: Ir): Either[UnexpectedTypeErr[_], T] =
    ty match
      case typed : T => Right(typed)
      case _         => Left(UnexpectedTypeErr[T](ty, node))
