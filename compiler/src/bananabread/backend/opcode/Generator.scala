package bananabread
package backend.opcode

import ir.typeless
import ir.typeless.Ir

import parsing.opcode.Tree => OpcodeTree
import parsing.opcode.Expr => OpcodeExpr
import parsing.opcode.Instruction => InstructionExpr

import runtime.value
import runtime.register._
import runtime.instruction
import runtime.instruction._
import runtime.instruction.{Value, Label, Instruction}
import runtime.instruction.{Type, I32, Str, Symbol}

import utils.ListOfEitherImplicits

import scala.util.Random
import scala.collection.mutable.{Map, Queue}


type Output = List[Grouped | Value | Label]
type Result = Either[GeneratorError, Output]

case class Grouped(section: String, data: Instruction | Label)

sealed trait GeneratorError
case class BadPushErr(ty: Type, node: Ir) extends GeneratorError
case class BadCallErr(lambda: Ir) extends GeneratorError
case class OpcodeSyntaxErr(source: parsing.ast.Str, err: parsing.error.SyntaxErr) extends GeneratorError
case class UndeclaredIdentifierErr(id: typeless.Id) extends GeneratorError
case class CannotStoreDefErr(definition: Ir) extends GeneratorError
case class UnknownUserOpcodeErr(expr: OpcodeExpr) extends GeneratorError


def generate(nodes: List[Ir]): Result =
  generate(backend.opcode.Scope.empty, nodes)
def generate(scope: Scope, nodes: List[Ir]): Result =
  nodes.map { node => generate(scope, node) }.squished
       .map { inst => inst.flatten }
def generate(scope: Scope, node: Ir): Result = node match
  case num: typeless.Num    => generatePush(scope, num, I32)
  case str: typeless.Str    => generatePush(scope, str, Str)
  case sym: typeless.Symbol => generatePush(scope, sym, Symbol)
  case id: typeless.Id      => generateLoad(scope, id)
  case typeless.App(lambda, args, _)      => generateCall(scope, lambda, args)
  case typeless.Cond(cond, pass, fail, _) => generateCond(scope, cond, pass, fail)
  case typeless.Let(bindings, body, _)    => generateLet(scope, bindings, body)
  case typeless.Begin(irs,_ )             => generateBegin(scope, irs)
  case typeless.Def(name, value, _)       => generateDef(scope, name.lexeme, value)

def generatePush(scope: Scope, node: Ir, ty: Type): Result = (ty, node) match
  case (I32, num: typeless.Num) =>
    Right(group(scope, Push(I32, num.num.lexeme)))
  case (Str, str: typeless.Str) =>
    Right(Value(Str, str.ptr, str.str.lexeme) +:
          group(scope, Push(Const, str.ptr)))
  case (Symbol, sym: typeless.Symbol) =>
    Right(Value(Symbol, sym.ptr, sym.symbol.lexeme) +:
          group(scope, Push(Str, sym.ptr)))
  case _ =>
    Left(BadPushErr(ty, node))

def generateLoad(scope: Scope, id: typeless.Id): Result = scope.get(id) match
  case Some(lam: typeless.Lambda) => Right(group(scope, Load(Ptr, lam.ptr)))
  case Some(_)                    => Right(group(scope, Load(I32, scope.qualified(id))))
  case None                       => Left(UndeclaredIdentifierErr(id))

def generateCall(scope: Scope, lambda: Ir, args: List[Ir]): Result = lambda match
  case id: typeless.Id if scope.contains(id) =>
    scope.get(id) match
      case Some(id: typeless.Id)      => generateCallId(scope, args, id)
      case Some(app: typeless.App)    => generateCallApp(scope, args, app)
      case Some(lam: typeless.Lambda) => generateCallLambda(scope, args, lam)
      case Some(_)                    => Left(BadCallErr(lambda))
      case None                       => Left(UndeclaredIdentifierErr(id))

  case typeless.Id(parsing.ast.Id("opcode", _)) => args match
    case typeless.Str(node @ parsing.ast.Str(str, _)) :: Nil =>
      parsing.opcode.parse("<opcode>", str) match
        case Right(tr) => generateOpcode(scope, tr)
        case Left(err) => Left(OpcodeSyntaxErr(node, err))
    case _             => Left(BadCallErr(lambda))

  case lam: typeless.Lambda =>
    for
      call <- generateCallLambda(scope, args, lam)
      func <- generate(scope, lam)
    yield
      call ++ func

  case id: typeless.Id      => generateCallId(scope, args, id)
  case app: typeless.App    => generateCallApp(scope, args, app)
  case _                    => Left(BadCallErr(lambda))

def generateOpcode(scope: Scope, tree: OpcodeTree): Result =
  tree.nodes.map(generateOpcode(scope, _)).squished.map(_.flatten)
def generateOpcode(scope: Scope, expr: OpcodeExpr): Result = expr match
  case InstructionExpr("add", Some("I32"), Nil, _) => Right(group(scope, Add(I32)))
  case InstructionExpr("sub", Some("I32"), Nil, _) => Right(group(scope, Sub(I32)))
  case InstructionExpr("concat", Some("Str"), Nil, _) => Right(group(scope, Concat))
  case InstructionExpr("load", Some("I32"), List(label), _) => Right(group(scope, Load(I32, scope.qualified(label))))
  case InstructionExpr("load", Some("Str"), List(label), _) => Right(group(scope, Load(Str, scope.qualified(label))))
  case InstructionExpr("println", None, Nil, _) => Right(group(scope, Println))
  case _ => Left(UnknownUserOpcodeErr(expr))

def generateCallId(scope: Scope, args: List[Ir], id: typeless.Id): Result =
  generateCallWithArgs(scope, args, Call(scope.qualified(id)))

def generateCallLambda(scope: Scope, args: List[Ir], lambda: typeless.Lambda): Result =
  generateCallWithArgs(scope, args, Call(lambda.ptr))

def generateCallApp(scope: Scope, args: List[Ir], app: typeless.App): Result =
  for
    call1 <- generateCall(scope, app.lambda, app.args)
    mov    = group(scope, Mov(Jm))
    call2 <- generateCallWithArgs(scope, args, Call0)
    codes  = call1 ++ mov ++ call2
  yield
    codes

def generateCallWithArgs(scope: Scope, args: List[Ir], call: Instruction): Result =
  generateCallArgsLoad(scope, args).map { header =>
    header ++ group(scope, call)
  }

def generateCallArgsLoad(scope: Scope, args: List[Ir]): Result =
  args.map(generate(scope, _)).squished.map { instructions =>
    instructions.flatten ++ group(scope, PushReg(Pc, value.I32(2)))
  }

def generateCond(scope: Scope, cond: Ir, pass: Ir, fail: Ir): Result =
  val condString = uniqueString(scope, "cond")
  val thenString = uniqueString(scope, "then")
  val elseString = uniqueString(scope, "else")
  val doneString = uniqueString(scope, "done")

  val thenLabel = group(scope, Label(thenString))
  val elseLabel = group(scope, Label(elseString))
  val doneLabel = group(scope, Label(doneString))

  val elseJump = group(scope, Jz(elseString))
  val doneJump = group(scope, Jmp(doneString))

  for
    condCode <- generate(scope, cond)
    passCode <- generate(scope, pass)
    failCode <- generate(scope, fail)
  yield
    condCode  ++ elseJump ++              // if
    thenLabel ++ passCode ++ doneJump ++  // then
    elseLabel ++ failCode ++              // else
    doneLabel                             // rest

def generateLet(scope: Scope, bindings: List[typeless.Binding], body: Ir): Result =
  val header = bindings.map { case binding @ typeless.Binding(label, value, _) =>
    scope.define(binding)
    for
      valueCode <- generate(scope, value)
      storeCode <- generateStore(scope, label.lexeme, value)
    yield
      valueCode ++ storeCode
  }

  for
    lets <- header.squished
    body <- generate(scope, body)
  yield
    lets.flatten ++ body

def generateDef(scope: Scope, name: String, value: Ir): Result = value match
  case lam: typeless.Lambda =>
    scope.define(name, value)
    scope.scoped(name) { subscope =>
      lam.params.foreach { param =>
        subscope.define(param, param)
      }

      generateLambda(subscope, lam.params, lam.body)
        .map(Label(lam.ptr) +: _ :+ Value(Ptr, scope.qualified(name), lam.ptr))
    }

def generateLambda(scope: Scope, params: List[Ir], body: Ir): Result =
  val header = params.reverse.flatMap { case typeless.Id(label) =>
    group(scope, Swap, Store(I32, scope.qualified(label)))
  }

  val footer = group(scope, Swap, Ret)
  generate(scope, body).map(header ++ _ ++ footer)

def generateStore(scope: Scope, label: String, value: Ir): Result = value match
  case _: typeless.Lambda => Right(group(scope, Store(Ptr, scope.qualified(label))))
  case _: typeless.Num    => Right(group(scope, Store(I32, scope.qualified(label))))
  case _: typeless.Str    => Right(group(scope, Store(Str, scope.qualified(label))))
  case _: typeless.Begin  => Right(group(scope, Store(I32, scope.qualified(label)))) /* XXX */
  case _: typeless.Let    => Right(group(scope, Store(I32, scope.qualified(label)))) /* XXX */
  case _: typeless.Cond   => Right(group(scope, Store(I32, scope.qualified(label)))) /* XXX */
  case _: typeless.App    => Right(group(scope, Store(I32, scope.qualified(label)))) /* XXX */
  case _: typeless.Symbol => Right(group(scope, Store(Symbol, scope.qualified(label))))
  case _: typeless.Def    => Left(CannotStoreDefErr(value))
  case id: typeless.Id => scope.get(id) match
    case Some(v) => generateStore(scope, label, v)
    case None    => Left(UndeclaredIdentifierErr(id))

def generateBegin(scope: Scope, irs: List[Ir]): Result =
  for
    codes <- irs.map { ir => generate(scope, ir) }.squished
  yield
    codes.flatten

def group(scope: Scope, insts: (Instruction | Label)*): Output =
  insts.toList.map { inst =>
    Grouped(scope.module, inst)
  }

def uniqueString(scope: Scope, label: String): String =
  s"$label-${Random.alphanumeric.take(4).mkString}"


extension (output: Output)
  def ordered: List[Code] =
    val sections = Map[String, Queue[Code]]()
    val values = Queue[Value]()

    output.foreach {
      case Grouped(section, inst: Instruction) =>
        sections.get(section) match
          case Some(q) => q.addOne(inst)
          case None    => sections.update(section, Queue(inst))
      case Grouped(section, label: Label) =>
      case value: Value => values.addOne(value)
      case label: Label =>
    }

    (for (section, instructions) <- sections if section == "main"
     yield Label(section) +: instructions).flatten.toList ++
    List(Halt) ++
    (for (section, instructions) <- sections if section != "main"
     yield Label(section) +: instructions).flatten ++
    values
