package bananabread
package ir
package linked

import parsing.ast
import program.ModDef
import ast.{Expr, Stmt}
import error._
import utils.{Print, squished}


sealed trait Ir { def expr: Expr | Stmt }
case class Num(expr: ast.Num) extends Ir with Print(s"(num ${expr.lexeme})")
case class Str(expr: ast.Str) extends Ir with Print(s"(str ${expr.lexeme})")
case class Id(expr: ast.Id, source: ModDef) extends Ir with Print(s"(id ${source.name}.${expr.lexeme})")
case class Symbol(expr: ast.Symbol) extends Ir with Print(s"(symbol ${expr.lexeme})")
case class App(lambda: Ir, args: List[Ir], expr: Expr) extends Ir with Print(s"(app lambda: ${lambda} args: (${args.mkString(" ")}))")
case class Lambda(params: List[ast.Param], body: Ir, tyVars: List[ast.TyId], expr: ast.Lambda) extends Ir with Print(s"(lambda params: (${params.mkString(" ")}) body: $body)")
case class Cond(cond: Ir, pass: Ir, fail: Ir, expr: Expr) extends Ir with Print(s"(if cond: $cond then: $pass else: $fail)")
case class Begin(ins: List[Ir], expr: Expr) extends Ir with Print(s"(begin ${ins.mkString(" ")})")
case class Opcode(expr: ast.Opcode) extends Ir with Print("opcode { ... }")
case class Def(name: ast.Id, value: Ir, expr: Stmt) extends Ir with Print(s"(def $name $value)")

case class Let(bindings: List[Binding], body: Ir, expr: Expr) extends Ir with Print(s"(let bindings: (${bindings.mkString(" ")}) body: $body)")
case class Binding(label: ast.Id, value: Ir, expr: ast.Binding) extends Print(s"binding: $label value: $value")

sealed trait Bool extends Ir
case class True(expr: ast.True) extends Bool, Print("true")
case class False(expr: ast.False) extends Bool, Print("false")


type Locals    = List[ast.Id]
type Lifted[T] = Either[LiftErr, T]
type Scoped[T] = Lifted[(T, Locals)]


def lift(nodes: List[typeless.Ir], module: Option[ast.Module], imports: List[ast.Import]): Lifted[List[Ir]] =
  nodes.foldLeft[Scoped[List[Ir]]](Right((List.empty, List.empty))) {
    case (Left(err), _) =>
      Left(err)
    case (Right((irs, locals)), node) =>
      lift(node, module, imports, locals).map { (lifted, newLocals) =>
        (irs :+ lifted, newLocals)
      }
  }.map(_._1)
def lift(node: typeless.Ir, module: Option[ast.Module], imports: List[ast.Import], locals: List[ast.Id]): Scoped[Ir] = node match
  case typeless.Num(expr)    => Right((Num(expr), locals))
  case typeless.Str(expr)    => Right((Str(expr), locals))
  case typeless.True(expr)   => Right((True(expr), locals))
  case typeless.False(expr)  => Right((False(expr), locals))
  case typeless.Symbol(expr) => Right((Symbol(expr), locals))
  case typeless.Opcode(expr) => Right((Opcode(expr), locals))
  case node: typeless.Id     => liftId(node, module, imports, locals)
  case node: typeless.App    => liftApp(node, module, imports, locals)
  case node: typeless.Lambda => liftLambda(node, module, imports, locals)
  case node: typeless.Cond   => liftCond(node, module, imports, locals)
  case node: typeless.Let    => liftLet(node, module, imports, locals)
  case node: typeless.Begin  => liftBegin(node, module, imports, locals)
  case node: typeless.Def    => liftDef(node, module, imports, locals)

def liftId(node: typeless.Id, module: Option[ast.Module], imports: List[ast.Import], locals: List[ast.Id]): Scoped[Id] =
  val id = node.expr.lexeme
  val local = locals.map(_.lexeme).contains(id)
  val source = imports.find { stmt =>
    stmt.exposing.map(_.id.lexeme).contains(id)
  }

  (local, module, source) match
    case (true, Some(mod), _) => Right((Id(node.expr, ModDef.from(mod)), locals))
    case (true, None, _) => Right((Id(node.expr, ModDef.main), locals))
    case (_, _, Some(imp)) => Right((Id(node.expr, ModDef.from(imp)), locals))
    case (_, _, _) => Left(UndeclaredIdentifierErr(node))

def liftApp(node: typeless.App, module: Option[ast.Module], imports: List[ast.Import], locals: List[ast.Id]): Scoped[App] =
  for
    liftedLamda <- lift(node.lambda, module, imports, locals).map(_._1)
    liftedArgs  <- node.args.map(lift(_, module, imports, locals)).squished.map(_.map(_._1))
  yield
    (App(liftedLamda, liftedArgs, node.expr), locals)

def liftLambda(node: typeless.Lambda, module: Option[ast.Module], imports: List[ast.Import], locals: List[ast.Id]): Scoped[Lambda] =
  for
    liftedBody <- lift(node.body, module, imports, locals ++ node.params.map(_.name)).map(_._1)
  yield
    (Lambda(node.params, liftedBody, node.tyVars, node.expr), locals)

def liftCond(node: typeless.Cond, module: Option[ast.Module], imports: List[ast.Import], locals: List[ast.Id]): Scoped[Cond] =
  for
    liftedCond <- lift(node.cond, module, imports, locals).map(_._1)
    liftedPass <- lift(node.pass, module, imports, locals).map(_._1)
    liftedFail <- lift(node.fail, module, imports, locals).map(_._1)
  yield
    (Cond(liftedCond, liftedPass, liftedFail, node.expr), locals)

def liftLet(node: typeless.Let, module: Option[ast.Module], imports: List[ast.Import], locals: List[ast.Id]): Scoped[Let] =
  for
    bindingsLift <- liftLetBindings(node, module, imports, locals)
    liftedBindings = bindingsLift._1._1
    recLocals = bindingsLift._1._2
    liftedBody <- lift(node.body, module, imports, recLocals).map(_._1)
  yield
    (Let(liftedBindings, liftedBody, node.expr), locals)

def liftLetBindings(node: typeless.Let, module: Option[ast.Module], imports: List[ast.Import], locals: List[ast.Id]): Scoped[(List[Binding], List[ast.Id])] =
  node.bindings.foldLeft[Scoped[(List[Binding], List[ast.Id])]](Right(((List.empty, locals), locals))) {
    case (Left(err), _) =>
      Left(err)
    case (Right(((liftedBindings, locals), _)), binding) =>
      val recLocals = locals :+ binding.label
      lift(binding.value, module, imports, recLocals).map(_._1).map { ir =>
        ((liftedBindings :+ Binding(binding.label, ir, binding.expr), recLocals), locals)
      }
  }

def liftBegin(node: typeless.Begin, module: Option[ast.Module], imports: List[ast.Import], locals: List[ast.Id]): Scoped[Begin] =
  for
    liftedIns <- node.ins.map(lift(_, module, imports, locals)).squished.map(_.map(_._1))
  yield
    (Begin(liftedIns, node.expr), locals)

def liftDef(node: typeless.Def, module: Option[ast.Module], imports: List[ast.Import], locals: List[ast.Id]): Scoped[Def] =
  for
    liftedValue <- lift(node.value, module, imports, locals :+ node.name).map(_._1)
  yield
    (Def(node.name, liftedValue, node.expr), locals :+ node.name)
