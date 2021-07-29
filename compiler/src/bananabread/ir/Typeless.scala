package bananabread
package ir.typeless

import parsing.ast
import error._
import ast.{Tree, Expr, Stmt}
import utils.{Ptr, PtrWith, Print}

import utils.{onlys, squished}


type Lifted[T] = Either[LiftErr, T]


sealed trait Ir { def expr: Expr | Stmt }
case class Num(expr: ast.Num) extends Ir with Print(s"(num ${expr.lexeme})")
case class Str(expr: ast.Str) extends Ir with Print(s"(str ${expr.lexeme})"), PtrWith("str", () => expr.lexeme.hashCode)
case class Id(expr: ast.Id) extends Ir with Print(s"(id ${expr.lexeme})")
case class Symbol(expr: ast.Symbol) extends Ir with Print(s"(symbol ${expr.lexeme})"), Ptr("symbol")
case class App(lambda: Ir, args: List[Ir], expr: Expr) extends Ir with Print(s"(app lambda: ${lambda} args: (${args.mkString(" ")}))")
case class Lambda(params: List[Id], body: Ir, tyVars: List[ast.Ty], expr: ast.Lambda) extends Ir with Print(s"(lambda params: (${params.mkString(" ")}) body: $body)"), Ptr("lambda")
case class Cond(cond: Ir, pass: Ir, fail: Ir, expr: Expr) extends Ir with Print(s"(if cond: $cond then: $pass else: $fail)")
case class Begin(ins: List[Ir], expr: Expr) extends Ir with Print(s"(begin ${ins.mkString(" ")})")
case class Def(name: ast.Id, value: Ir, expr: Stmt) extends Ir with Print(s"(def $name $value)")

case class Let(bindings: List[Binding], body: Ir, expr: Expr) extends Ir with Print(s"(let bindings: (${bindings.mkString(" ")}) body: $body)")
case class Binding(label: ast.Id, value: Ir, expr: ast.Binding) extends Print(s"binding: $label value: $value")

sealed trait Bool extends Ir
case class True(expr: ast.True) extends Bool, Print("true")
case class False(expr: ast.False) extends Bool, Print("false")


def lift(tree: Tree): Lifted[List[Ir]] =
  tree.nodes.map(lift).squished
def lift(binding: ast.Binding): Lifted[Binding] =
  for
    valueIr <- lift(binding.value)
  yield
    Binding(binding.label, valueIr, binding)
def lift(node: Stmt | Expr): Lifted[Ir] = node match
  case expr: ast.Num    => Right(Num(expr))
  case expr: ast.Str    => Right(Str(expr))
  case expr: ast.True   => Right(True(expr))
  case expr: ast.False  => Right(False(expr))
  case expr: ast.Id     => Right(Id(expr))
  case expr: ast.Symbol => Right(Symbol(expr))
  case expr: ast.App    => liftApp(expr)
  case expr: ast.Lambda => liftLambda(expr)
  case expr: ast.Uniop  => liftUniop(expr)
  case expr: ast.Binop  => liftBinop(expr)
  case expr: ast.Cond   => liftCond(expr)
  case expr: ast.Let    => liftLet(expr)
  case expr: ast.Begin  => liftBegin(expr)
  case stmt: ast.Def    => liftDef(stmt)

def liftApp(app: ast.App): Lifted[App] =
  for
    lambdaIr <- lift(app.lambda)
    argsIrs  <- app.args.map(lift).squished
  yield
    App(lambdaIr, argsIrs, app)

def liftLambda(lambda: ast.Lambda): Lifted[Lambda] =
  for
    paramsIr <- lambda.params.map(_.name).map(lift).squished
    bodyIr   <- lift(lambda.body)
  yield
    paramsIr.onlys[Id] match
      case Right(ids) =>
        Lambda(ids, bodyIr, lambda.tyVars, lambda)
      case _ => return Left(BadParamIdentifier(paramsIr))

def liftUniop(app: ast.Uniop): Lifted[App] =
  for
    opIr  <- lift(app.op)
    argIr <- lift(app.operand)
  yield
    App(opIr, List(argIr), app)

def liftBinop(app: ast.Binop): Lifted[App] =
  for
    opIr    <- lift(app.op)
    argsIrs <- List(app.lhs, app.rhs).map(lift).squished
  yield
    App(opIr, argsIrs, app)

def liftCond(cond: ast.Cond): Lifted[Cond] =
  for
    condIr <- lift(cond.cond)
    passIr <- lift(cond.pass)
    failIr <- lift(cond.fail)
  yield
    Cond(condIr, passIr, failIr, cond)

def liftLet(let: ast.Let): Lifted[Let] =
  for
    bindingsIr <- let.bindings.map(lift).squished
    bodyIr     <- lift(let.body)
  yield
    Let(bindingsIr, bodyIr, let)

def liftBegin(begin: ast.Begin): Lifted[Begin] =
  for
    irs <- (begin.head +: begin.tail).map(lift).squished
  yield
    Begin(irs, begin)

def liftDef(defStmt: ast.Def): Lifted[Def] =
  for
    valueIr <- lift(defStmt.value)
  yield
    Def(defStmt.name, valueIr, defStmt)
