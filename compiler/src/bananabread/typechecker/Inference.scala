package bananabread
package typechecker

import error._
import ty._
import unification.Substitution
import ir.typeless
import ir.typeless.Ir
import parsing.ast

import utils.squished

import scala.reflect.ClassTag


type Scoped[T] = Inferred[(T, Scope)]
type Inferred[T] = Either[InferenceErr, T]

type Scope = Map[String, Type]
object Scope:
  def empty: Scope =
    Map.empty

  def from(params: List[ast.Param], tyScope: TypeScope): Inferred[Scope] =
    params.foldLeft[Inferred[Scope]](Right(Map.empty)) {
      case (Left(err), param) => Left(err)
      case (Right(scope), ast.Param(name, None)) => Right(scope + (name.lexeme -> fresh()))
      case (Right(scope), ast.Param(name, Some(tag))) =>
        processType(tag, tyScope) match
          case Left(err) => Left(err)
          case Right(ty) => Right(scope + (name.lexeme -> ty))
    }

type TypeScope = Map[String, Type]
object TypeScope:
  def empty: TypeScope =
    Map.empty

  def from(tyVars: List[ast.TyId]): TypeScope =
    tyVars.map { tyVar => (tyVar.id.lexeme, fresh()) }.toMap


def infer(nodes: List[Ir], scope: Scope, sub: Substitution): Scoped[List[Type]] =
  nodes.foldLeft[Scoped[List[Type]]](Right((List.empty, scope))) {
    case (Left(err), _) =>
      Left(err)
    case (Right((tys, scope)), node) =>
      infer(node, scope, sub) match
        case Left(err) => Left(err)
        case Right((ty, next)) =>
          Right((tys :+ ty, next))
  }
def infer(node: Ir, scope: Scope, sub: Substitution): Scoped[Type] = node match
  case _: typeless.Num     => Right((ty.I32, scope))
  case _: typeless.Str     => Right((ty.Str, scope))
  case _: typeless.Symbol  => Right((ty.Symbol, scope))
  case _: typeless.Bool    => Right((ty.Bool, scope))
  case _: typeless.Opcode  => Right((ty.Void, scope))
  case ir: typeless.Id     => inferId(ir, scope, sub)
  case ir: typeless.Begin  => inferBegin(ir, scope, sub)
  case ir: typeless.Def    => inferDef(ir, scope, sub)
  case ir: typeless.App    => inferApp(ir, scope, sub)
  case ir: typeless.Lambda => inferLambda(ir, scope, sub)
  case ir: typeless.Cond   => inferCond(ir, scope, sub)
  case ir: typeless.Let    => inferLet(ir, scope, sub)

def inferId(id: typeless.Id, scope: Scope, sub: Substitution): Scoped[Type] =
  lookup(id, id.expr.lexeme, scope).map { ty => (ty, scope) }

def inferBegin(begin: typeless.Begin, scope: Scope, sub: Substitution): Scoped[Type] =
  infer(begin.ins.last, scope, sub)

def inferDef(defIr: typeless.Def, scope: Scope, sub: Substitution): Scoped[Type] =
  inferDef(defIr.name.lexeme, defIr.value, scope, sub)
def inferDef(bindingIr: typeless.Binding, scope: Scope, sub: Substitution): Scoped[Type] =
  inferDef(bindingIr.label.lexeme, bindingIr.value, scope, sub)
def inferDef(label: String, value: Ir, scope: Scope, sub: Substitution): Scoped[Type] =
  infer(value, scope, sub).map { (ty, scope) =>
    (ty, scope + (label -> ty))
  }

def inferApp(app: typeless.App, scope: Scope, sub: Substitution): Scoped[Type] =
  for
    inferredArgs <- app.args.map { arg => infer(arg, scope, sub) }.squished
    argTys = inferredArgs.map(_._1)
    freshLam = Lambda(argTys, fresh())
    called <- infer(app.lambda, scope, sub)
    calledTy = called._1
    _ <- sub.unify(calledTy, freshLam, app)
    culprit = if app.args.isEmpty
              then app
              else app.args.head
    unifiedTy <- sub(calledTy, culprit)
    fnTy   <- unifiedTy.expect[Lambda](app.lambda)
  yield
    (fnTy.app(argTys.size), scope)

/** TODO Handle type variables
  */
def inferLambda(lam: typeless.Lambda, scope: Scope, sub: Substitution): Scoped[Lambda] =
  val tyScope = TypeScope.from(lam.tyVars)

  for
    params <- inferLambdaParams(lam, scope, sub, tyScope)
    ret    <- inferLambdaRet(params._1, lam, scope, sub, params._2)
  yield
    (Lambda(params._1, ret._1), scope)

def inferLambdaParams(
  lam: typeless.Lambda,
  scope: Scope,
  sub: Substitution,
  tyScope: TypeScope,
): Inferred[(List[Type], TypeScope)] =
  val tags = lam.expr.params.map(_.ty)
  val params = lam.params

  params.zip(tags).foldLeft[Inferred[(List[Type], TypeScope)]](Right((List.empty, tyScope))) {
    case (Right((acc, _)), (param, None)) =>
      Right((acc :+ fresh(), tyScope))

    case (Right((acc, _)), (param, Some(tag))) =>
      processType(tag, tyScope) match
        case Right(ty) => Right((acc :+ ty, tyScope))
        case Left(err) => Left(err)

    case (Left(err), _) =>
      Left(err)
  }

/** TODO Substitute type variables
  */
def inferLambdaRet(
  paramTys: List[Type],
  lam: typeless.Lambda,
  scope: Scope,
  sub: Substitution,
  tyScope: TypeScope,
): Inferred[(Type, TypeScope)] =
  lam.expr.tyRet match
    case Some(tag) =>
      processType(tag, tyScope) match
        case Right(ty) => Right((ty, tyScope))
        case Left(err) => Left(err)

    case None =>
      val lexicalScope = lam.expr.params
        .map { param => param.name.lexeme }
        .zip(paramTys)
        .foldLeft(scope) { case (scope, (name, ty)) => scope + (name -> ty) }

      for
        ty <- infer(lam.body, lexicalScope, sub)
      yield
        (ty._1, tyScope)

/** TODO Ensure condTy unifies with Bool.
  */
def inferCond(cond: typeless.Cond, scope: Scope, sub: Substitution): Scoped[Type] =
  for
    condTy <- infer(cond.cond, scope, sub)
    passTy <- infer(cond.pass, scope, sub)
    failTy <- infer(cond.fail, scope, sub)
    _      <- sub.unify(passTy._1, failTy._1, cond.fail)
    exprTy <- sub(failTy._1, cond.fail)
  yield
    (exprTy, scope)

def inferLet(let: typeless.Let, scope: Scope, sub: Substitution): Scoped[Type] =
  val subscope = let.bindings.foldLeft(scope) { (scope, binding) =>
    inferDef(binding, scope, sub) match
      case Right((_, scope)) => scope
      case Left(err) => return Left(err)
  }

  infer(let.body, subscope, sub)


def signature(lam: typeless.Lambda): Inferred[ty.Lambda] =
  for
    params <- signatureLambdaParams(lam, TypeScope.from(lam.tyVars))
    ret    <- signatureLambdaRet(lam, params._2)
  yield
    Lambda(params._1, ret._1)

def signatureLambdaParams(lam: typeless.Lambda, tyScope: TypeScope): Inferred[(List[Type], TypeScope)] =
  lam.params.foldLeft[Inferred[(List[Type], TypeScope)]](Right((List.empty, tyScope))) {
    case (Left(err), param) => Left(err)
    case (Right((tys, scope)), ast.Param(name, None)) =>
      val ty = fresh()
      Right((tys :+ ty, scope + (name.lexeme -> ty)))
    case (Right((tys, scope)), ast.Param(name, Some(tag))) =>
      processType(tag, scope) match
        case Left(err) => Left(err)
        case Right(ty) => Right((tys :+ ty, scope + (name.lexeme -> ty)))
  }

def signatureLambdaRet(lam: typeless.Lambda, tyScope: TypeScope): Inferred[(Type, TypeScope)] =
  lam.expr.tyRet match
    case Some(tag) =>
      processType(tag, tyScope) match
        case Right(ty) => Right((ty, tyScope))
        case Left(err) => Left(err)
    case None =>
      Right((fresh(), tyScope))


def lookup(ir: Ir, label: String, scope: Scope): Either[LookupErr, Type] =
  scope.get(label) match
    case None => Left(LookupErr(label, ir))
    case Some(ty) => Right(ty)

val ids = LazyList.from(1).sliding(1)
def fresh() =
  Var(ids.next.head)


def processType(tag: ast.Ty, tyScope: TypeScope): Either[UnknowTypeErr, Type] =
  tag match
    case node @ ast.TyId(id) =>
      (id.lexeme, tyScope.get(id.lexeme)) match
        case (_, Some(ty)) => Right(ty)
        case ("I32", _)    => Right(ty.I32)
        case ("Str", _)    => Right(ty.Str)
        case ("Symbol", _) => Right(ty.Symbol)
        case ("Void", _)   => Right(ty.Void)
        case ("Bool", _)   => Right(ty.Bool)
        case (baddie, _)   => Left(UnknowTypeErr(node))
    case ast.TyLamda(params, retTy) =>
      for
        parsedParams <- params.map { param => processType(param, tyScope) }.squished
        parsedRet    <- processType(retTy, tyScope)
      yield
        Lambda(parsedParams, parsedRet)


extension (ty: Type)
  def ensure(expected: Type, node: Ir): Either[TypeMismatchErr, Type] =
    if ty == expected
    then Right(ty)
    else Left(TypeMismatchErr(expected, ty, node))

  def expect[T <: Type : ClassTag](node: Ir): Either[UnexpectedTypeErr[_], T] =
    ty match
      case typed : T => Right(typed)
      case _         => Left(UnexpectedTypeErr[T](ty, node))
