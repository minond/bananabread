package bananabread
package ir.typeless

import parsing.ast
import error._
import ast.{Tree, Expr, Stmt}
import utils.{Ptr, Print}

import utils.{onlys, squished}


type Lifted[T] = Either[LiftErr, T]


sealed trait Ir
case class Num(num: ast.Num) extends Ir with Print(s"(num ${num.lexeme})")
case class Str(str: ast.Str) extends Ir with Print(s"(str ${str.lexeme})"), Ptr("str")
case class Id(id: ast.Id) extends Ir with Print(s"(id ${id.lexeme})")
case class Symbol(symbol: ast.Symbol) extends Ir with Print(s"(symbol ${symbol.lexeme})"), Ptr("symbol")
case class App(lambda: Ir, args: List[Ir], expr: Expr) extends Ir with Print(s"(app lambda: ${lambda} args: (${args.mkString(" ")}))")
case class Lambda(params: List[Id], body: Ir, expr: Expr) extends Ir with Print(s"(lambda params: (${params.mkString(" ")}) body: $body)"), Ptr("lambda")
case class Cond(cond: Ir, pass: Ir, fail: Ir, expr: Expr) extends Ir with Print(s"(if cond: $cond then: $pass else: $fail)")
case class Let(bindings: List[Binding], body: Ir, expr: Expr) extends Ir with Print(s"(let bindings: (${bindings.mkString(" ")}) body: $body)")
case class Begin(ins: List[Ir], expr: Expr) extends Ir with Print(s"(begin ${ins.mkString(" ")})")
case class Def(name: ast.Id, value: Ir, stmt: Stmt) extends Ir with Print(s"(def $name $value)")

case class Binding(label: ast.Id, value: Ir, expr: ast.Binding) extends Print(s"binding: $label value: $value")

def lift(tree: Tree): Lifted[List[Ir]] =
  tree.nodes.map(lift).squished
def lift(binding: ast.Binding): Lifted[Binding] =
  for
    valueIr <- lift(binding.value)
  yield
    Binding(binding.label, valueIr, binding)
def lift(node: Stmt | Expr): Lifted[Ir] = node match
  case expr: ast.Num => Right(Num(expr))
  case expr: ast.Str => Right(Str(expr))
  case expr: ast.Id => Right(Id(expr))
  case expr: ast.Symbol => Right(Symbol(expr))
  case expr @ ast.App(lambda, args) =>
    for
      lambdaIr <- lift(lambda)
      argsIrs  <- args.map(lift).squished
    yield
      App(lambdaIr, argsIrs, expr)
  case expr @ ast.Lambda(params, body, _, _) =>
    for
      paramsIr <- params.map(_.name).map(lift).squished
      bodyIr   <- lift(body)
    yield
      paramsIr.onlys[Id] match
        case Right(ids) =>
          Lambda(ids, bodyIr, expr)
        case _ => return Left(BadParamIdentifier(paramsIr))
  case expr @ ast.Uniop(op, operand) =>
    for
      opIr  <- lift(op)
      argIr <- lift(operand)
    yield
      App(opIr, List(argIr), expr)
  case expr @ ast.Binop(op, lhs, rhs) =>
    for
      opIr    <- lift(op)
      argsIrs <- List(lhs, rhs).map(lift).squished
    yield
      App(opIr, argsIrs, expr)
  case expr @ ast.Cond(_, cond, pass, fail) =>
    for
      condIr <- lift(cond)
      passIr <- lift(pass)
      failIr <- lift(fail)
    yield
      Cond(condIr, passIr, failIr, expr)
  case expr @ ast.Let(_, bindings, body) =>
    for
      bindingsIr <- bindings.map(lift).squished
      bodyIr     <- lift(body)
    yield
      Let(bindingsIr, bodyIr, expr)
  case expr @ ast.Begin(head, tail) =>
    for
      irs <- (head +: tail).map(lift).squished
    yield
      Begin(irs, expr)
  case stmt @ ast.Def(name, value) =>
    for
      valueIr <- lift(value)
    yield
      Def(name, valueIr, stmt)
