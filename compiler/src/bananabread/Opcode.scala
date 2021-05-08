package bananabread
package opcode

import runtime.value

import ir.Typeless => tl
import ir.Typeless.Ir
import value.Value
import utils.Print

import scala.util.Random
import scala.collection.mutable.{Map, Queue, Stack}


type Instructions = Queue[Instruction]


sealed trait Type
case object I32 extends Type
case object Reg extends Type
case object Ptr extends Type
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
case class Add(typ: Type) extends Opcode with Print(s"add [$typ]"), Run(vm => vm.bini32op(_ + _))
case class Sub(typ: Type) extends Opcode with Print(s"sub [$typ]"), Run(vm => vm.bini32op(_ - _))

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


class Scope(env: Map[String, Ir] = Map.empty, parent: Option[Scope] = None):
  private val children = Stack[Scope]()

  def contains(label: String): Boolean = (env.contains(label), parent) match
    case (true, _) => true
    case (_, Some(scope)) => scope.contains(label)
    case _ => false

  def lookup(label: String): Ir = (env.get(label), parent) match
    case (Some(ir), _) => ir
    case (_, Some(scope)) => scope.lookup(label)
    case _ => ???

  def get(label: String): Option[Ir] = (env.get(label), parent) match
    case (Some(ir), _) => Some(ir)
    case (_, Some(scope)) => scope.get(label)
    case _ => None

  def define(label: String, ir: Ir) =
    env.update(label, ir)
    this

  def subscope =
    val child = Scope(Map.empty, Some(this))
    children.push(child)
    child

  def scoped(fn: Scope => Unit) =
    fn(subscope)


class Emitter(
  section: String = "Main",
  sections: Map[String, Instructions] = Map("Main" -> Queue.empty),
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
    inst(Label, value.Id("Main")) ++
    sections.get("Main").get ++
    inst(Halt) ++
    (for (sec, instructions) <- sections if sec != "Main"
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
  val s = Scope()
  nodes.foreach(compile(_, e, s))
  e

def compile(node: Ir, e: Emitter, s: Scope): Emitter =
  node match
    case _: tl.Num => push(node, ty.I32, e, s)
    case _: tl.Str => push(node, ty.Str, e, s)
    case _: tl.Symbol => push(node, ty.Symbol, e, s)
    case v: tl.Lambda =>
      // XXX 1
      s.scoped { scope =>
        v.params.foreach { param => scope.define(param.id.lexeme, param) }
        lambda(v.params, v.body, e.to(v.ptr), scope)
        e.emit(inst(Push(Ptr), name(v.ptr)))
      }
    case tl.Id(parsing.ast.Id(label, _)) => load(label, e, s)
    case tl.App(lambda, args, _) => call(lambda, args, e, s)
    case tl.Cond(cnd, pas, fal, _) => cond(cnd, pas, fal, e, s)
    case tl.Let(bindings, body, _) => let(bindings, body, e, s.subscope)
    case tl.Begin(ins, _) => begin(ins, e, s)
    case tl.Def(name, value, _) => define(name.lexeme, value, e, s)
  e


def push(node: Ir, typ: ty.Type, e: Emitter, s: Scope) = (typ, value.lift(node), node) match
  case (ty.I32, v, _) => e.emit(inst(Push(I32), v))
  case (ty.Symbol, v : value.Symbol, sym : tl.Symbol) =>
    e.symbol(sym.ptr, v)
    e.emit(inst(Push(Const), value.Id(sym.ptr)))
  case (ty.Symbol, _, _) => ???
  case (ty.Str, v : value.Str, str : tl.Str) =>
    e.string(str.ptr, v)
    e.emit(inst(Push(Const), value.Id(str.ptr)))
  case (ty.Str, _, _) => ???
  case (_: ty.Var, _, _) => ???
  case (_: ty.Lambda, _, _) => ???

def name(name: String): value.Id =
  value.Id(name)

def unique(name: String): value.Id =
  value.Id(s"$name-${Random.alphanumeric.take(16).mkString}")

def call(lambda: Ir, args: List[Ir], e: Emitter, s: Scope): Unit = lambda match
  case tl.Id(parsing.ast.Id(label, _)) if Exposed.contains(label) =>
    args.foreach(compile(_, e, s))
    e.emit(inst(Exposed.lookup(label)))
  case tl.Id(parsing.ast.Id(label, _)) if s.contains(label) =>
    s.lookup(label) match
      case lambda: tl.Lambda =>
        loadArgsAndRet(args, e, s)
        e.emit(inst(Call, name(lambda.ptr)))
      case id: tl.Id =>
        loadArgsAndRet(args, e, s)
        // e.emit(inst(Call, value.lift(id)))
        e.emit(inst(Call, value.lift(lambda)))
      case _ =>
        ???
  case tl.Id(parsing.ast.Id("opcode", _)) => args match
    case tl.Str(str) :: Nil =>
      println(str)
    case _ =>
      /* bad call */
      ???
  case tl.Id(parsing.ast.Id(label, _)) =>
    loadArgsAndRet(args, e, s)
    e.emit(inst(Call, value.lift(lambda)))
  case lambda: tl.Lambda =>
    loadArgsAndRet(args, e, s)
    e.emit(inst(Call, name(lambda.ptr)))
    compile(lambda, e.to(lambda.ptr), s)
  case app: tl.App =>
    call(app.lambda, app.args, e, s)
    e.emit(inst(Mov, runtime.vm.Reg.Jmp))
    loadArgsAndRet(args, e, s)
    e.emit(inst(Call))
  case _ =>
    /* bad call */
    ???

def loadArgsAndRet(args: List[Ir], e: Emitter, s: Scope) =
  args.foreach(compile(_, e, s))
  e.emit(inst(Push(Reg), runtime.vm.Reg.Pc, value.I32(2)))

def load(label: String, e: Emitter, s: Scope) = s.get(label) match
  case None => ???
  case Some(lambda : tl.Lambda) => e.emit(inst(Load(Ptr), name(s"Main.$label")))
  case Some(_) => e.emit(inst(Load(I32), name(label)))

def store(label: String, e: Emitter, s: Scope) =
  e.emit(inst(Store(I32), name(label)))

def storev(label: String, v: Ir, e: Emitter, s: Scope): Unit = v match
  case _: tl.Num => e.emit(inst(Store(I32), name(label)))
  case _: tl.Str => ???
  case tl.Id(id) => storev(label, s.lookup(id.lexeme), e, s)
  case _: tl.Symbol => ???
  case _: tl.Def => ???
  case v: tl.Lambda =>
    // XXX 1
    // e.emit(inst(Push(Ptr), name(v.ptr)))
    e.emit(inst(Store(Ptr), name(label)))
  case _: tl.App =>
    // TODO App's result may not be i32, need to pass ty.Type instead of
    // typeless Ir.
    e.emit(inst(Store(I32), name(label)))
  case _: tl.Cond =>
    // TODO Cond's result may not be i32, need to pass ty.Type instead of
    // typeless Ir.
    e.emit(inst(Store(I32), name(label)))
  case _: tl.Let =>
    // TODO Let's result may not be i32, need to pass ty.Type instead of
    // typeless Ir.
    e.emit(inst(Store(I32), name(label)))
  case _: tl.Begin =>
    // TODO Begin's result may not be i32, need to pass ty.Type instead of
    // typeless Ir.
    e.emit(inst(Store(I32), name(label)))

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

def let(bindings: List[tl.Binding], body: Ir, e: Emitter, s: Scope) =
  bindings.foreach { case tl.Binding(parsing.ast.Id(label, _), v, _) =>
    s.define(label, v)
    compile(v, e, s)
    storev(label, v, e, s)
  }
  compile(body, e, s)

def begin(ins: List[Ir], e: Emitter, s: Scope) =
  ins.foreach { ir =>
    compile(ir, e, s)
  }

def lambda(params: List[tl.Id], body: Ir, e: Emitter, s: Scope) =
  params.reverse.foreach { case tl.Id(parsing.ast.Id(label, _)) =>
    e.emit(inst(Swap))
    store(label, e, s)
  }
  compile(body, e, s)
  e.emit(inst(Swap))
  e.emit(inst(Ret))

def define(name: String, rawValue: Ir, e: Emitter, s: Scope): Unit = rawValue match
  case v: tl.Lambda =>
    // TODO don't hardcode module
    e.pointer(s"Main.$name", value.Id(v.ptr))
    s.define(name, v)
    s.scoped { scope =>
      v.params.foreach { param => scope.define(param.id.lexeme, param) }
      lambda(v.params, v.body, e.to(v.ptr), scope)
    }

  case _ =>
    s.define(name, rawValue)
    compile(rawValue, e, s)
    storev(name, rawValue, e, s)
