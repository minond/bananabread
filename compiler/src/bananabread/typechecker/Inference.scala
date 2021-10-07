package bananabread
package typechecker

import error._
import ty._
import unification.Substitution
import ir.linked
import ir.linked.Ir
import parsing.ast
import program.{ModuleSpace, ModDef, search}

import utils.squished

import scala.reflect.{ClassTag, classTag}


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


def infer(nodes: List[Ir], scope: Scope, sub: Substitution, space: ModuleSpace): Scoped[List[Type]] =
  nodes.foldLeft[Scoped[List[Type]]](Right((List.empty, scope))) {
    case (Left(err), _) =>
      Left(err)
    case (Right((tys, scope)), node) =>
      infer(node, scope, sub, space) match
        case Left(err) => Left(err)
        case Right((ty, next)) =>
          Right((tys :+ ty, next))
  }
def infer(node: Ir, scope: Scope, sub: Substitution, space: ModuleSpace): Scoped[Type] = node match
  case _: linked.Num     => Right((ty.I32, scope))
  case _: linked.Str     => Right((ty.Str, scope))
  case _: linked.Symbol  => Right((ty.Symbol, scope))
  case _: linked.Bool    => Right((ty.Bool, scope))
  case _: linked.Opcode  => Right((ty.Void, scope))
  case ir: linked.Id     => inferId(ir, scope, sub, space)
  case ir: linked.Begin  => inferBegin(ir, scope, sub, space)
  case ir: linked.Def    => inferDef(ir, scope, sub, space)
  case ir: linked.App    => inferApp(ir, scope, sub, space)
  case ir: linked.Lambda => inferLambda(ir, scope, sub, space)
  case ir: linked.Cond   => inferCond(ir, scope, sub, space)
  case ir: linked.Let    => inferLet(ir, scope, sub, space)

def inferId(id: linked.Id, scope: Scope, sub: Substitution, space: ModuleSpace): Scoped[Type] =
  lookup(id, id.expr.lexeme, id.source, scope, space).map { ty => (ty, scope) }

def inferBegin(begin: linked.Begin, scope: Scope, sub: Substitution, space: ModuleSpace): Scoped[Type] =
  infer(begin.ins.last, scope, sub, space)

def inferDef(defIr: linked.Def, scope: Scope, sub: Substitution, space: ModuleSpace): Scoped[Type] =
  inferDef(defIr.name.lexeme, defIr.value, scope, sub, space)
def inferDef(bindingIr: linked.Binding, scope: Scope, sub: Substitution, space: ModuleSpace): Scoped[Type] =
  inferDef(bindingIr.label.lexeme, bindingIr.value, scope, sub, space)
def inferDef(label: String, value: Ir, scope: Scope, sub: Substitution, space: ModuleSpace): Scoped[Type] =
  infer(value, scope, sub, space).map { (ty, scope) =>
    (ty, scope + (label -> ty))
  }

def inferApp(app: linked.App, scope: Scope, sub: Substitution, space: ModuleSpace): Scoped[Type] =
  for
    inferredArgs <- app.args.map { arg => infer(arg, scope, sub, space) }.squished
    argTys = inferredArgs.map(_._1)
    freshLam = Lambda(argTys, fresh())
    called <- infer(app.lambda, scope, sub, space)
    calledTy = called._1
    _ <- sub.unify(freshLam, calledTy, app)
    culprit = if app.args.isEmpty
              then app
              else app.args.head
    unifiedTy <- sub(calledTy, culprit)
    fnTy <- unifiedTy.expect[Lambda](app.lambda)
  yield
    (fnTy.app(argTys.size), scope)

/** TODO Handle type variables
  */
def inferLambda(lam: linked.Lambda, scope: Scope, sub: Substitution, space: ModuleSpace): Scoped[Lambda] =
  val tyScope = TypeScope.from(lam.tyVars)

  for
    params <- inferLambdaParams(lam, scope, sub, tyScope, space)
    ret    <- inferLambdaRet(params._1, lam, scope, sub, params._2, space)
  yield
    (Lambda(params._1, ret._1), scope)

def inferLambdaParams(
  lam: linked.Lambda,
  scope: Scope,
  sub: Substitution,
  tyScope: TypeScope,
  space: ModuleSpace,
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
  lam: linked.Lambda,
  scope: Scope,
  sub: Substitution,
  tyScope: TypeScope,
  space: ModuleSpace,
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
        ty <- infer(lam.body, lexicalScope, sub, space)
      yield
        (ty._1, tyScope)

/** TODO Ensure condTy unifies with Bool.
  */
def inferCond(cond: linked.Cond, scope: Scope, sub: Substitution, space: ModuleSpace): Scoped[Type] =
  for
    condTy <- infer(cond.cond, scope, sub, space)
    passTy <- infer(cond.pass, scope, sub, space)
    failTy <- infer(cond.fail, scope, sub, space)
    _      <- sub.unify(passTy._1, failTy._1, cond.fail)
    exprTy <- sub(failTy._1, cond.fail)
  yield
    (exprTy, scope)

def inferLet(let: linked.Let, scope: Scope, sub: Substitution, space: ModuleSpace): Scoped[Type] =
  val subscope = let.bindings.foldLeft(scope) { (scope, binding) =>
    inferDef(binding, scope, sub, space) match
      case Right((_, scope)) => scope
      case Left(err) => return Left(err)
  }

  infer(let.body, subscope, sub, space)


def signature(lam: linked.Lambda): Inferred[ty.Lambda] =
  for
    params <- signatureLambdaParams(lam, TypeScope.from(lam.tyVars))
    ret    <- signatureLambdaRet(lam, params._2)
  yield
    Lambda(params._1, ret._1)

def signatureLambdaParams(lam: linked.Lambda, tyScope: TypeScope): Inferred[(List[Type], TypeScope)] =
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

def signatureLambdaRet(lam: linked.Lambda, tyScope: TypeScope): Inferred[(Type, TypeScope)] =
  lam.expr.tyRet match
    case Some(tag) =>
      processType(tag, tyScope) match
        case Right(ty) => Right((ty, tyScope))
        case Left(err) => Left(err)
    case None =>
      Right((fresh(), tyScope))


def lookup(ir: Ir, label: String, source: ModDef, scope: Scope, space: ModuleSpace): Either[LookupErr, Type] =
  (scope.get(label), space.search(source, label)) match
    case (None, None)  => Left(LookupErr(label, ir))
    case (_, Some(ir)) => Right(ir.ty)
    case (Some(ty), _) => Right(ty)

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
  def expect(expected: Type, node: Ir): Either[TypeMismatchErr, Type] =
    if ty == expected
    then Right(ty)
    else Left(TypeMismatchErr(expected, ty, node))

  def expect[T <: Type : ClassTag](node: Ir): Either[GenTypeMismatchErr[_], T] =
    ty match
      case typed : T => Right(typed)
      case _         => Left(GenTypeMismatchErr(classTag[T].runtimeClass, ty, node))
