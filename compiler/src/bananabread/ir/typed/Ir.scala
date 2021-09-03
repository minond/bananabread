package bananabread
package ir.typed

import parsing.ast
import parsing.ast.Expr
import typechecker.ty
import typechecker.ty.Type


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
