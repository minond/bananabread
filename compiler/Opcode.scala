package bananabread
package opcode

import ir.Typeless => tl
import ir.Typeless.Ir
import value.Value
import utils.Print

import scala.util.Random
import scala.collection.mutable.{Map, Queue}


type Instructions = Queue[Instruction]


sealed trait Opcode
case object Halt extends Opcode with Print("halt")
case object Label extends Opcode
case object Jz extends Opcode with Print("jz")
case object Jmp extends Opcode with Print("jmp")
case object PushI32 extends Opcode with Print("push_i32")
case object PushPtr extends Opcode with Print("push_ptr")
case object PushReg extends Opcode with Print("push_reg")
case object Call extends Opcode with Print("call")
case object Run extends Opcode with Print("run")
case object Ret extends Opcode with Print("ret")
case object Swap extends Opcode with Print("swap")
case object Mov extends Opcode with Print("mov")
case object LoadI32 extends Opcode with Print("load_i32") // local to stack
case object StoreI32 extends Opcode with Print("store_i32") // stack to local
case object StorePtr extends Opcode with Print("store_ptr") // stack to local


case class Scope(env: Map[String, Ir] = Map.empty, parent: Option[Scope] = None):
  def contains(label: String): Boolean = (env.contains(label), parent) match
    case (true, _) => true
    case (_, Some(scope)) => scope.contains(label)
    case _ => false

  def lookup(label: String): Ir = (env.get(label), parent) match
    case (Some(ir), _) => ir
    case (_, Some(scope)) => scope.lookup(label)
    case _ => ???

  def define(label: String, ir: Ir) =
    env.update(label, ir)

  def child =
    Scope(Map.empty, Some(this))


class Emitter(section: String = "main", sections: Map[String, Instructions] = Map.empty):
  def to(section: String) =
    Emitter(section, sections)

  def emit(i: Instruction): Emitter =
    sections.get(section) match
      case Some(sec) => sec.addOne(i)
      case None => sections.update(section, Queue(i))
    this

  def emit(is: List[Instruction]): Emitter =
    is.map(emit)
    this

  def dump =
    inst(opcode.Label, value.Id("main")) ++
    sections.get("main").get ++
    inst(opcode.Halt) ++
    (for (sec, instructions) <- sections if sec != "main"
     yield inst(opcode.Label, value.Id(sec)) ++ instructions).flatten


case class Instruction(op: Opcode, args: Value*):
  def toList = List(this)

  override def toString = op match
    case opcode.Label => args(0).toString
      // if args(0).toString.startsWith("lambda")
      // then s"\n${args(0)}:"
      // else s"${args(0)}:"
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
    case _: tl.Str => ???
    case v: tl.Lambda =>
      // XXX 1
      lambda(v.params, v.body, e.to(v.ptr), s)
      e.emit(inst(opcode.PushPtr, name(v.ptr)))
    case tl.Id(ast.Id(label, _)) => load(label, e, s)
    case tl.App(lambda, args, _) => call(lambda, args, e, s)
    case tl.Cond(cnd, pas, fal, _) => cond(cnd, pas, fal, e, s)
    case tl.Let(bindings, body, _) => let(bindings, body, e, s)
  e


def push(node: Ir, typ: ty.Type, e: Emitter, s: Scope) = typ match
  case ty.I32 => e.emit(inst(opcode.PushI32, value.lift(node)))
  case ty.Str => ???
  case _: ty.Var => ???
  case _: ty.Lambda => ???

def name(name: String): value.Id =
  value.Id(name)

def unique(name: String): value.Id =
  value.Id(s"$name-${Random.alphanumeric.take(16).mkString}")

def call(lambda: Ir, args: List[Ir], e: Emitter, s: Scope): Unit = lambda match
  case tl.Id(ast.Id(label, _)) if Seq("+", "-").contains(label) =>
    args.foreach(compile(_, e, s))
    e.emit(inst(opcode.Run, name(label)))
  case tl.Id(ast.Id(label, _)) if s.contains(label) =>
    s.lookup(label) match
      case lambda: tl.Lambda =>
        loadArgsAndRet(args, e, s)
        e.emit(inst(opcode.Call, name(lambda.ptr)))
      case _ =>
        loadArgsAndRet(args, e, s)
        e.emit(inst(opcode.Call, value.lift(lambda)))
  case tl.Id(ast.Id(label, _)) =>
    loadArgsAndRet(args, e, s)
    e.emit(inst(opcode.Call, value.lift(lambda)))
  case lambda: tl.Lambda =>
    loadArgsAndRet(args, e, s)
    e.emit(inst(opcode.Call, name(lambda.ptr)))
    compile(lambda, e.to(lambda.ptr), s)
  case app: tl.App =>
    call(app.lambda, app.args, e, s)
    e.emit(inst(opcode.Mov, vm.Reg.Jmp))
    loadArgsAndRet(args, e, s)
    e.emit(inst(opcode.Call))
  case _ =>
    /* bad call */
    ???

def loadArgsAndRet(args: List[Ir], e: Emitter, s: Scope) =
  e.emit(inst(opcode.PushReg, vm.Reg.Pc, value.I32(args.size + 2)))
  args.foreach(compile(_, e, s))

def load(label: String, e: Emitter, s: Scope) =
  e.emit(inst(opcode.LoadI32, name(label)))

def store(label: String, e: Emitter, s: Scope) =
  e.emit(inst(opcode.StoreI32, name(label)))

def storev(label: String, v: Ir, e: Emitter, s: Scope) = v match
  case _: tl.Num => e.emit(inst(opcode.StoreI32, name(label)))
  case _: tl.Str => ???
  case _: tl.Id => ???
  case v: tl.Lambda =>
    // XXX 1
    // e.emit(inst(opcode.PushPtr, name(v.ptr)))
    e.emit(inst(opcode.StorePtr, name(label)))
  case _: tl.App =>
    // TODO App's result may not be i32, need to pass ty.Type instead of
    // typeless Ir.
    e.emit(inst(opcode.StoreI32, name(label)))
  case _: tl.Cond =>
    // TODO Cond's result may not be i32, need to pass ty.Type instead of
    // typeless Ir.
    e.emit(inst(opcode.StoreI32, name(label)))
  case _: tl.Let =>
    // TODO Let's result may not be i32, need to pass ty.Type instead of
    // typeless Ir.
    e.emit(inst(opcode.StoreI32, name(label)))

def cond(cnd: Ir, pas: Ir, fal: Ir, e: Emitter, s: Scope) =
  val lcond = unique("cond")
  val lthen = unique("then")
  val lelse = unique("else")
  val ldone = unique("done")

  e.emit(inst(opcode.Label, lcond))
  compile(cnd, e, s)
  e.emit(inst(opcode.Jz, lelse))
  e.emit(inst(opcode.Label, lthen))
  compile(pas, e, s)
  e.emit(inst(opcode.Jmp, ldone))
  e.emit(inst(opcode.Label, lelse))
  compile(fal, e, s)
  e.emit(inst(opcode.Label, ldone))

def let(bindings: List[tl.Binding], body: Ir, e: Emitter, s: Scope) =
  bindings.foreach { case tl.Binding(ast.Id(label, _), v, _) =>
    s.define(label, v)
    compile(v, e, s)
    storev(label, v, e, s)
  }
  compile(body, e, s)

def lambda(params: List[tl.Id], body: Ir, e: Emitter, s: Scope) =
  params.reverse.foreach { case tl.Id(ast.Id(label, _)) =>
    store(label, e, s)
  }
  compile(body, e, s)
  e.emit(inst(opcode.Swap))
  e.emit(inst(opcode.Ret))