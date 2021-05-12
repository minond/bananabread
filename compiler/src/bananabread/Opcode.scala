package bananabread
package opcode

import runtime.value

import ir.typeless
import ir.typeless.Ir
import value.Value
import utils.Print

import scala.util.Random
import scala.collection.mutable.{Map, Queue, Stack}


type Instructions = Queue[Instruction]


sealed trait Type
case object I32 extends Type
case object Str extends Type
case object Reg extends Type
case object Ptr extends Type
case object Scope extends Type
case object Const extends Type


sealed trait Opcode
case object Halt extends Opcode with Print("halt")
case object Label extends Opcode
case object Value extends Opcode
case object Jz extends Opcode with Print("jz")
case object Jmp extends Opcode with Print("jmp")
case class Push(typ: Type) extends Opcode with Print(s"push [$typ]")
case object Call extends Opcode with Print("call")
case object Ret extends Opcode with Print("ret")
case object Swap extends Opcode with Print("swap")
case object Mov extends Opcode with Print("mov")
case class Load(typ: Type) extends Opcode with Print(s"load [$typ]") // local to stack
case class Store(typ: Type) extends Opcode with Print(s"store [$typ]") // stack to local

sealed trait Run(val handler: runtime.vm.Machine => Unit)
case object Println extends Opcode with Print("println"), Run(vm => println(vm.stack.head))
case class Add(typ: Type) extends Opcode with Print(s"add [$typ]"), Run(vm => vm.binI32Op(_ + _))
case class Sub(typ: Type) extends Opcode with Print(s"sub [$typ]"), Run(vm => vm.binI32Op(_ - _))
case class Concat(typ: Type) extends Opcode with Print(s"concat [$typ]"), Run(vm => vm.binStrOp(_ + _))

object Exposed:
  val registry = Map(
    "println" -> Println,
    "+" -> Add(I32),
    "-" -> Sub(I32),
  )

  def contains(label: String) =
    registry.keys.toSeq.contains(label)

  def lookup(label: String) =
    registry(label)


class Scope(val module: String, env: Map[String, Ir] = Map.empty, parent: Option[Scope] = None):
  private val children = Stack[Scope]()

  def contains(label: String): Boolean = (env.contains(label), parent) match
    case (true, _) => true
    case (_, Some(scope)) => scope.contains(label)
    case _ => false

  def lookup(label: String): Ir = (env.get(label), parent) match
    case (Some(ir), _) => ir
    case (_, Some(scope)) => scope.lookup(label)
    case _ => ???

  def container(label: String): Scope = (env.get(label), parent) match
    case (Some(_), _) => this
    case (_, Some(scope)) => scope.container(label)
    case _ => ???

  def get(label: String): Option[Ir] = (env.get(label), parent) match
    case (Some(ir), _) => Some(ir)
    case (_, Some(scope)) => scope.get(label)
    case _ => None

  def define(label: String, ir: Ir) =
    env.update(label, ir)
    this

  def subscope(suffix: String) =
    val child = Scope(s"$module.$suffix", Map.empty, Some(this))
    children.push(child)
    child

  def scoped(suffix: String)(fn: Scope => Unit) =
    fn(subscope(suffix))


class Emitter(
  section: String = "main",
  sections: Map[String, Instructions] = Map("main" -> Queue.empty),
  strings: Map[String, value.Str] = Map.empty,
  symbols: Map[String, value.Symbol] = Map.empty,
  pointers: Map[String, value.Id] = Map.empty,
):
  def to(section: String) =
    Emitter(section, sections, strings, symbols, pointers)

  def emit(i: Instruction): Emitter =
    sections.get(section) match
      case Some(sec) => sec.addOne(i)
      case None => sections.update(section, Queue(i))
    this

  def emit(is: List[Instruction]): Emitter =
    is.map(emit)
    this

  def string(label: String, str: value.Str) =
    strings.update(label, str)

  def symbol(label: String, sym: value.Symbol) =
    symbols.update(label, sym)

  def pointer(label: String, ptr: value.Id) =
    pointers.update(label, ptr)

  def dump =
    inst(Label, value.Id("main")) ++
    sections.get("main").get ++
    inst(Halt) ++
    (for (sec, instructions) <- sections if sec != "main"
     yield inst(Label, value.Id(sec)) ++ instructions).flatten ++
    (for (label, str) <- strings
     yield inst(Value, value.Id(label), value.Id("Str"), str)).flatten ++
    (for (label, str) <- symbols
     yield inst(Value, value.Id(label), value.Id("Symbol"), str)).flatten ++
    (for (label, ptr) <- pointers
     yield inst(Value, value.Id(label), value.Id("Ptr"), ptr)).flatten


case class Instruction(op: Opcode, args: Value*):
  def toList = List(this)

  override def toString = op match
    case Label => s"${args(0)}:"
    case Value => s"${args(0)} [${args(1)}]: ${args(2)}"
    case _ => s"  $op ${args.mkString(", ")}"


def inst(op: Opcode, args: Value*) =
  Instruction(op, args:_*).toList


def compile(nodes: List[Ir]): Emitter =
  val e = Emitter()
  val s = Scope("main")
  nodes.foreach(compile(_, e, s))
  e
def compile(node: Ir, e: Emitter, s: Scope): Emitter =
  node match
    case _: typeless.Num => push(node, ty.I32, e, s)
    case _: typeless.Str => push(node, ty.Str, e, s)
    case _: typeless.Symbol => push(node, ty.Symbol, e, s)
    case v: typeless.Lambda =>
      // XXX 1
      s.scoped(rand) { scope =>
        v.params.foreach { param => scope.define(param.id.lexeme, param) }
        lambda(v.params, v.body, e.to(v.ptr), scope)
        e.emit(inst(Push(Scope), name(v.ptr)))
      }
    case typeless.Id(parsing.ast.Id(label, _)) => load(label, e, s)
    case typeless.App(lambda, args, _) => call(lambda, args, e, s)
    case typeless.Cond(cnd, pas, fal, _) => cond(cnd, pas, fal, e, s)
    case typeless.Let(bindings, body, _) => let(bindings, body, e, s.subscope(rand))
    case typeless.Begin(ins, _) => begin(ins, e, s)
    case typeless.Def(name, value, _) => define(name.lexeme, value, e, s)
  e

def push(node: Ir, typ: ty.Type, e: Emitter, s: Scope) = (typ, value.lift(node), node) match
  case (ty.I32, v, _) => e.emit(inst(Push(I32), v))
  case (ty.Symbol, v : value.Symbol, sym : typeless.Symbol) =>
    e.symbol(sym.ptr, v)
    e.emit(inst(Push(Const), value.Id(sym.ptr)))
  case (ty.Symbol, _, _) => ???
  case (ty.Str, v : value.Str, str : typeless.Str) =>
    e.string(str.ptr, v)
    e.emit(inst(Push(Const), value.Id(str.ptr)))
  case (ty.Str, _, _) => ???
  case (_: ty.Var, _, _) => ???
  case (_: ty.Lambda, _, _) => ???

def name(name: String): value.Id =
  value.Id(name)

def unique(name: String): value.Id =
  value.Id(s"$name-${Random.alphanumeric.take(16).mkString}")

def rand =
  Random.alphanumeric.take(4).mkString

def call(lambda: Ir, args: List[Ir], e: Emitter, s: Scope): Unit = lambda match
  case typeless.Id(parsing.ast.Id(label, _)) if Exposed.contains(label) =>
    args.foreach(compile(_, e, s))
    e.emit(inst(Exposed.lookup(label)))
  case typeless.Id(parsing.ast.Id(label, _)) if s.contains(label) =>
    s.lookup(label) match
      case lambda: typeless.Lambda =>
        loadArgsAndRet(args, e, s)
        e.emit(inst(Call, name(lambda.ptr)))
      case id: typeless.Id =>
        loadArgsAndRet(args, e, s)
        // e.emit(inst(Call, value.lift(id)))
        e.emit(inst(Call, name(s"${s.container(label).module}.$label")))
      case app: typeless.App =>
        call(app.lambda, app.args, e, s)
        e.emit(inst(Mov, runtime.vm.Reg.Jmp))
        loadArgsAndRet(args, e, s)
        e.emit(inst(Call))
      case _ =>
        ???
  case typeless.Id(parsing.ast.Id("opcode", _)) => args match
    case typeless.Str(parsing.ast.Str(str, _)) :: Nil =>
      parsing.opcode.parse("<opcode>", str).map { tree => opcodes(tree.nodes, e, s) }
    case _ =>
      /* bad call */
      ???
  case typeless.Id(parsing.ast.Id(label, _)) =>
    loadArgsAndRet(args, e, s)
    e.emit(inst(Call, value.lift(lambda)))
  case lambda: typeless.Lambda =>
    loadArgsAndRet(args, e, s)
    e.emit(inst(Call, name(lambda.ptr)))
    compile(lambda, e.to(lambda.ptr), s)
  case app: typeless.App =>
    call(app.lambda, app.args, e, s)
    e.emit(inst(Mov, runtime.vm.Reg.Jmp))
    loadArgsAndRet(args, e, s)
    e.emit(inst(Call))
  case _ =>
    /* bad call */
    ???

def opcodes(instructions: List[parsing.opcode.Expr], e: Emitter, s: Scope): Emitter =
  instructions.foreach(opcodes(_, e, s))
  e
def opcodes(instruction: parsing.opcode.Expr, e: Emitter, s: Scope): Emitter = instruction match
  case _: parsing.opcode.Label => ???
  case _: parsing.opcode.Constant => ???
  case parsing.opcode.Instruction("add", Some("I32"), Nil, _) =>
    e.emit(inst(Add(I32)))
  case parsing.opcode.Instruction("concat", Some("Str"), Nil, _) =>
    e.emit(inst(Concat(Str)))
  case parsing.opcode.Instruction("load", Some("I32"), List(label), _) =>
    e.emit(inst(Load(I32), name(s"${s.container(label).module}.$label")))
  case parsing.opcode.Instruction("load", Some("Str"), List(label), _) =>
    e.emit(inst(Load(Str), name(s"${s.container(label).module}.$label")))
  case _: parsing.opcode.Instruction => ???

def loadArgsAndRet(args: List[Ir], e: Emitter, s: Scope) =
  args.foreach(compile(_, e, s))
  e.emit(inst(Push(Reg), runtime.vm.Reg.Pc, value.I32(2)))

def load(label: String, e: Emitter, s: Scope) = s.get(label) match
  case None => ???
  case Some(lambda : typeless.Lambda) => e.emit(inst(Load(Ptr), name(s"${s.container(label).module}.$label")))
  case Some(_) => e.emit(inst(Load(I32), name(s"${s.container(label).module}.$label")))

def store(label: String, e: Emitter, s: Scope) =
  e.emit(inst(Store(I32), name(s"${s.module}.$label")))

def storev(label: String, v: Ir, e: Emitter, s: Scope): Unit = v match
  case _: typeless.Num => e.emit(inst(Store(I32), name(s"${s.module}.$label")))
  case _: typeless.Str => e.emit(inst(Store(Str), name(s"${s.module}.$label")))
  case typeless.Id(id) => storev(label, s.lookup(id.lexeme), e, s)
  case _: typeless.Symbol => ???
  case _: typeless.Def => ???
  case v: typeless.Lambda =>
    // XXX 1
    // e.emit(inst(Push(Ptr), name(v.ptr)))
    e.emit(inst(Store(Ptr), name(s"${s.module}.$label")))
  case _: typeless.App =>
    // TODO App's result may not be i32, need to pass ty.Type instead of
    // typeless Ir.
    e.emit(inst(Store(I32), name(s"${s.module}.$label")))
  case _: typeless.Cond =>
    // TODO Cond's result may not be i32, need to pass ty.Type instead of
    // typeless Ir.
    e.emit(inst(Store(I32), name(s"${s.module}.$label")))
  case _: typeless.Let =>
    // TODO Let's result may not be i32, need to pass ty.Type instead of
    // typeless Ir.
    e.emit(inst(Store(I32), name(s"${s.module}.$label")))
  case _: typeless.Begin =>
    // TODO Begin's result may not be i32, need to pass ty.Type instead of
    // typeless Ir.
    e.emit(inst(Store(I32), name(s"${s.module}.$label")))

def cond(cnd: Ir, pas: Ir, fal: Ir, e: Emitter, s: Scope) =
  val lcond = unique("cond")
  val lthen = unique("then")
  val lelse = unique("else")
  val ldone = unique("done")

  e.emit(inst(Label, lcond))
  compile(cnd, e, s)
  e.emit(inst(Jz, lelse))
  e.emit(inst(Label, lthen))
  compile(pas, e, s)
  e.emit(inst(Jmp, ldone))
  e.emit(inst(Label, lelse))
  compile(fal, e, s)
  e.emit(inst(Label, ldone))

def let(bindings: List[typeless.Binding], body: Ir, e: Emitter, s: Scope) =
  bindings.foreach { case typeless.Binding(parsing.ast.Id(label, _), v, _) =>
    s.define(label, v)
    compile(v, e, s)
    storev(label, v, e, s)
  }
  compile(body, e, s)

def begin(ins: List[Ir], e: Emitter, s: Scope) =
  ins.foreach { ir =>
    compile(ir, e, s)
  }

def lambda(params: List[typeless.Id], body: Ir, e: Emitter, s: Scope) =
  params.reverse.foreach { case typeless.Id(parsing.ast.Id(label, _)) =>
    e.emit(inst(Swap))
    store(label, e, s)
  }
  compile(body, e, s)
  e.emit(inst(Swap))
  e.emit(inst(Ret))

def define(name: String, rawValue: Ir, e: Emitter, s: Scope): Unit = rawValue match
  case v: typeless.Lambda =>
    // TODO don't hardcode module
    e.pointer(s"${s.module}.$name", value.Id(v.ptr))
    s.define(name, v)
    s.scoped(name) { scope =>
      v.params.foreach { param => scope.define(param.id.lexeme, param) }
      lambda(v.params, v.body, e.to(v.ptr), scope)
    }

  case _ =>
    s.define(name, rawValue)
    compile(rawValue, e, s)
    storev(name, rawValue, e, s)
