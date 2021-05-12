package bananabread
package ir

import parsing.ast

import ast.{Tree, Expr, Stmt}
import ty.Type
import utils.{Print, ListImplicits}

import scala.collection.mutable.Map


sealed trait Ir(expr: Expr, ty: Type)
case class Num(num: ast.Num, ty: Type) extends Ir(num, ty)
case class Str(str: ast.Str) extends Ir(str, ty.Str)
case class Id(expr: Expr, ty: Type) extends Ir(expr, ty)
case class Symbol(id: ast.Id) extends Ir(id, ty.Symbol)
case class App(lambda: Ir, args: List[Ir], expr: Expr, ty: Type) extends Ir(expr, ty)
case class Lambda(params: List[Id], body: Ir, expr: Expr, ty: Type) extends Ir(expr, ty)
case class Cond(cond: Ir, pass: Ir, fail: Ir, expr: Expr, ty: Type) extends Ir(expr, ty)
case class Let(bindings: List[Binding], body: Ir, expr: Expr, ty: Type) extends Ir(expr, ty)

case class Binding(label: ast.Id, value: Ir, expr: ast.Binding, ty: Type)

val ids = LazyList.from(1).sliding(1)
val ptrs = Map[String, String]()
def ptrOf(header: String, hash: Int) = ptrs.get(header + hash.toString) match
  case Some(ptr) => ptr
  case None =>
    val ptr = s"$header-${ids.next.head}"
    ptrs.update(header + hash.toString, ptr)
    ptr


/** Simplifies the source tree by removing all operators, leaving only
  * literals, functions, and function application.
  */
object Typeless:
  sealed trait Ir
  case class Num(num: ast.Num) extends Ir with Print(s"(num ${num.lexeme})")
  case class Str(str: ast.Str) extends Ir with Print(s"(str ${str.lexeme})"):
    def ptr = ptrOf("str", hashCode)
  case class Id(id: ast.Id) extends Ir with Print(s"(id ${id.lexeme})")
  case class Symbol(symbol: ast.Symbol) extends Ir with Print(s"(symbol ${symbol.lexeme})"):
    def ptr = ptrOf("symbol", hashCode)
  case class App(lambda: Ir, args: List[Ir], expr: Expr) extends Ir with Print(s"(app lambda: ${lambda} args: (${args.mkString(" ")}))")
  case class Lambda(params: List[Id], body: Ir, expr: Expr) extends Ir with Print(s"(lambda params: (${params.mkString(" ")}) body: $body)"):
    def ptr = ptrOf("lambda", hashCode)
  case class Cond(cond: Ir, pass: Ir, fail: Ir, expr: Expr) extends Ir with Print(s"(if cond: $cond then: $pass else: $fail)")
  case class Let(bindings: List[Binding], body: Ir, expr: Expr) extends Ir with Print(s"(let bindings: (${bindings.mkString(" ")}) body: $body)")
  case class Begin(ins: List[Ir], expr: Expr) extends Ir with Print(s"(begin ${ins.mkString(" ")})")
  case class Def(name: ast.Id, value: Ir, stmt: Stmt) extends Ir with Print(s"(def $name $value)")

  case class Binding(label: ast.Id, value: Ir, expr: ast.Binding) extends Print(s"binding: $label value: $value")

  def lift(tree: Tree): List[Ir] =
    tree.nodes.map(lift)

  def lift(node: Stmt | Expr): Ir = node match
    case expr: ast.Num => Num(expr)
    case expr: ast.Str => Str(expr)
    case expr: ast.Id => Id(expr)
    case expr: ast.Symbol => Symbol(expr)
    case expr @ ast.App(lambda, args) => App(lift(lambda), args.map(lift), expr)
    case expr @ ast.Lambda(params, body) =>
      params.map(lift).onlys[Id] match
        case Left(_) => ???
        case Right(ids) => Lambda(ids, lift(body), expr)
    case expr @ ast.Uniop(op, operand) => App(lift(op), List(operand).map(lift), expr)
    case expr @ ast.Binop(op, lhs, rhs) => App(lift(op), List(lhs, rhs).map(lift), expr)
    case expr @ ast.Cond(_, cond, pass, fail) => Cond(lift(cond), lift(pass), lift(fail), expr)
    case expr @ ast.Let(_, bindings, body) => Let(bindings.map(lift), lift(body), expr)
    case expr @ ast.Begin(head, tail) => Begin((head +: tail).map(lift), expr)
    case stmt @ ast.Def(name, value) => Def(name, lift(value), stmt)

  def lift(binding: ast.Binding): Binding =
    Binding(binding.label, lift(binding.value), binding)