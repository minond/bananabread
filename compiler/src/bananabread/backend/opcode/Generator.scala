package bananabread
package backend.opcode

import ir.typeless
import ir.typeless.Ir

import parsing.opcode.Tree => OpcodeTree
import parsing.opcode.Expr => OpcodeExpr
import parsing.opcode.Instruction => InstructionExpr

import runtime.value
import runtime.value.Id
import runtime.register._
import runtime.instruction
import runtime.instruction._
import runtime.instruction.{Value, Label, Instruction}
import runtime.instruction.{Type, I32, Str, Symbol}

import utils.{safeToInt, squished}

import scala.util.Random
import scala.collection.mutable.{Map, Queue}


case class Grouped(section: String, data: Instruction | Label)
type Output = List[Grouped | Value | Label]
type Result = Either[GeneratorError, Output]


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
  case lam: typeless.Lambda => generateAnnonLambda(scope, lam)
  case typeless.App(lambda, args, _)      => generateCall(scope, lambda, args)
  case typeless.Cond(cond, pass, fail, _) => generateCond(scope, cond, pass, fail)
  case typeless.Let(bindings, body, _)    => generateLet(scope, bindings, body)
  case typeless.Begin(irs,_ )             => generateBegin(scope, irs)
  case typeless.Def(name, value, _)       => generateDef(scope, name.lexeme, value)

def generatePush(scope: Scope, node: Ir, ty: Type): Result = (ty, node) match
  case (I32, num: typeless.Num) =>
    num.num.lexeme.safeToInt match
      case Left(_)  => Left(BadPushErr(ty, node))
      case Right(i) => Right(group(scope, Push(I32, value.I32(i))))
  case (Str, str: typeless.Str) =>
    Right(Value(Str, str.ptr, value.Str(str.str.lexeme)) +:
          group(scope, Push(Const, value.Id(str.ptr))))
  case (Symbol, sym: typeless.Symbol) =>
    Right(Value(Symbol, sym.ptr, value.Symbol(sym.symbol.lexeme)) +:
          group(scope, Push(Const, value.Id(sym.ptr))))
  case _ =>
    Left(BadPushErr(ty, node))

def generateLoad(scope: Scope, id: typeless.Id): Result = scope.get(id) match
  case Some(lam: typeless.Lambda) => Right(group(scope, Load(Ptr, lam.ptr)))
  case Some(_)                    => Right(group(scope, Load(I32, scope.qualified(id))))
  case None                       => Left(UndeclaredIdentifierErr(id))

def generateAnnonLambda(scope: Scope, lambda: typeless.Lambda): Result =
  scope.forked(lambda.ptr) { subscope =>
    val exposure =
      if scope.isToplevel
      then List.empty
      else group(scope, Push(Scope, value.Id(lambda.ptr)))

    generateLambda(subscope, lambda.params, lambda.body)
      .map(Label(lambda.ptr) +: _)
      .map(_ ++ exposure)
      .map(_ :+ Value(Ptr, lambda.ptr, Id(lambda.ptr)))
  }

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
  case InstructionExpr("add",     Some("I32"), Nil,         _) => Right(group(scope, Add(I32)))
  case InstructionExpr("sub",     Some("I32"), Nil,         _) => Right(group(scope, Sub(I32)))
  case InstructionExpr("push",    Some("I32"), List(str), _)   => withI32(expr, str) { i => group(scope, Push(I32, value.I32(i))) }
  case InstructionExpr("push",    Some("Str"), List(label), _) => Right(group(scope, Push(Str, value.Id(scope.qualified(label)))))
  case InstructionExpr("push",    Some("Ptr"), List(label), _) => Right(group(scope, Push(Ptr, value.Id(scope.qualified(label)))))
  case InstructionExpr("load",    Some("I32"), List(label), _) => Right(group(scope, Load(I32, scope.qualified(label))))
  case InstructionExpr("load",    Some("Str"), List(label), _) => Right(group(scope, Load(Str, scope.qualified(label))))
  case InstructionExpr("load",    Some("Ptr"), List(label), _) => Right(group(scope, Load(Ptr, scope.qualified(label))))
  case InstructionExpr("store",   Some("I32"), List(label), _) => Right(group(scope, Store(I32, scope.qualified(label))))
  case InstructionExpr("store",   Some("Str"), List(label), _) => Right(group(scope, Store(Str, scope.qualified(label))))
  case InstructionExpr("store",   Some("Ptr"), List(label), _) => Right(group(scope, Store(Ptr, scope.qualified(label))))
  case InstructionExpr("jz",      None,        List(label), _) => Right(group(scope, Jz(scope.qualified(label))))
  case InstructionExpr("jmp",     None,        List(label), _) => Right(group(scope, Jmp(scope.qualified(label))))
  case InstructionExpr("call",    None,        List(label), _) => Right(group(scope, Call(scope.qualified(label))))
  case InstructionExpr("mov",     Some("Pc"),  Nil,         _) => Right(group(scope, Mov(Pc, None)))
  case InstructionExpr("mov",     Some("Lr"),  Nil,         _) => Right(group(scope, Mov(Lr, None)))
  case InstructionExpr("mov",     Some("Jm"),  Nil,         _) => Right(group(scope, Mov(Jm, None)))
  case InstructionExpr("mov",     Some("Pc"),  List(str), _)   => withI32(expr, str) { i => group(scope, Mov(Pc, Some(value.I32(i)))) }
  case InstructionExpr("mov",     Some("Lr"),  List(str), _)   => withI32(expr, str) { i => group(scope, Mov(Lr, Some(value.I32(i)))) }
  case InstructionExpr("mov",     Some("Jm"),  List(str), _)   => withI32(expr, str) { i => group(scope, Mov(Jm, Some(value.I32(i)))) }
  case InstructionExpr("concat",  None,        Nil,         _) => Right(group(scope, Concat))
  case InstructionExpr("println", None,        Nil,         _) => Right(group(scope, Println))
  case InstructionExpr("halt",    None,        Nil,         _) => Right(group(scope, Halt))
  case InstructionExpr("call0",   None,        Nil,         _) => Right(group(scope, Call0))
  case InstructionExpr("ret",     None,        Nil,         _) => Right(group(scope, Ret))
  case InstructionExpr("swap",    None,        Nil,         _) => Right(group(scope, Swap))
  case _                                                       => Left(UnknownUserOpcodeErr(expr))

def generateCallId(scope: Scope, args: List[Ir], id: typeless.Id): Result =
  generateCallWithArgs(scope, args, Call(scope.qualified(id)))

def generateCallLambda(scope: Scope, args: List[Ir], lambda: typeless.Lambda): Result =
  generateCallWithArgs(scope, args, Call(lambda.ptr))

def generateCallApp(scope: Scope, args: List[Ir], app: typeless.App): Result =
  for
    call1 <- generateCall(scope, app.lambda, app.args)
    mov    = group(scope, Mov(Jm, None))
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
    instructions.flatten ++ group(scope, Mov(Pc, Some(value.I32(2))))
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
  val header = bindings.map { case typeless.Binding(label, value, _) =>
    scope.define(label, value)
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
    scope.define(name, lam)
    scope.forked(lam.ptr) { subscope =>
      generateLambda(subscope, lam.params, lam.body)
        .map(Label(lam.ptr) +: _ :+ Value(Ptr, scope.qualified(name), Id(lam.ptr)))
    }

  case _ =>
    scope.define(name, value)
    generate(scope, value)
      .map(_ ++ group(scope, Store(I32, scope.qualified(name))))

def generateLambda(scope: Scope, params: List[typeless.Id], body: Ir): Result =
  val header = params.reverse.flatMap { case param @ typeless.Id(label) =>
    scope.define(param, param)
    group(scope, Swap, Store(I32, scope.qualified(label)))
  }

  val footer = group(scope, Swap, Ret)
  generate(scope, body).map(header ++ _ ++ footer)

def generateStore(scope: Scope, label: String, value: Ir): Result = value match
  case _: typeless.Lambda => Right(group(scope, Store(Ptr, scope.qualified(label))))
  case _: typeless.Num    => Right(group(scope, Store(I32, scope.qualified(label))))
  case _: typeless.Str    => Right(group(scope, Store(Str, scope.qualified(label))))
  case _: typeless.Begin  => Right(group(scope, Store(I32, scope.qualified(label)))) /* XXX May not be an I32 */
  case _: typeless.Let    => Right(group(scope, Store(I32, scope.qualified(label)))) /* XXX May not be an I32 */
  case _: typeless.Cond   => Right(group(scope, Store(I32, scope.qualified(label)))) /* XXX May not be an I32 */
  case _: typeless.App    => Right(group(scope, Store(I32, scope.qualified(label)))) /* XXX May not be an I32 */
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

def withI32(expr: OpcodeExpr, str: String)(f: Int => Output): Result =
  str.safeToInt match
    case Left(_)  => Left(InvalidI32Err(expr))
    case Right(i) => Right(f(i))


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
        sections.get(section) match
          case Some(q) => q.addOne(label)
          case None    => sections.update(section, Queue(label))
      case value: Value => values.addOne(value)
      case label: Label =>
    }

    (for (section, instructions) <- sections if section == "main"
     yield Label(section) +: instructions).flatten.toList ++
    List(Halt) ++
    (for (section, instructions) <- sections if section != "main"
     yield Label(section) +: instructions).flatten ++
    values
