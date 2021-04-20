package sourdough
package runtime

import ir.Typeless => tl
import ir.Typeless.Ir
import opcode.Opcode
import value.Value

import scala.util.Random
import scala.collection.mutable.{Map, Queue}


type Instructions = Queue[Instruction]


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
    case opcode.Label =>
      if args(0).toString.startsWith("lambda")
      then s"\n${args(0)}:"
      else s"${args(0)}:"
    case _ => s"  $op ${args.mkString(", ")}"


def inst(op: Opcode, args: Value*) =
  Instruction(op, args:_*).toList


def lift(nodes: List[Ir]): Emitter =
  val e = Emitter()
  nodes.foreach(lift(_, e))
  e

def lift(node: Ir, e: Emitter): Emitter =
  node match
    case _: tl.Num => push(node, ty.I32, e)
    case _: tl.Str => ???
    case v: tl.Lambda =>
      // XXX 1
      lambda(v.params, v.body, e.to(v.ptr))
      e.emit(inst(opcode.PushPtr, name(v.ptr)))
    case tl.Id(ast.Id(label, _)) => load(label, e)
    case tl.App(lambda, args, _) => call(lambda, args, e)
    case tl.Cond(cnd, pas, fal, _) => cond(cnd, pas, fal, e)
    case tl.Let(bindings, body, _) => let(bindings, body, e)
  e


def push(node: Ir, typ: ty.Type, e: Emitter) = typ match
  case ty.I32 => e.emit(inst(opcode.PushI32, value.lift(node)))
  case ty.Str => ???
  case _: ty.Var => ???
  case _: ty.Lambda => ???

def name(name: String): value.Id =
  value.Id(name)

def unique(name: String): value.Id =
  value.Id(s"$name-${Random.alphanumeric.take(16).mkString}")

def call(lambda: Ir, args: List[Ir], e: Emitter): Unit = lambda match
  case tl.Id(ast.Id(label, _)) if label == "+" =>
    args.foreach(lift(_, e))
    e.emit(inst(opcode.Run, name(label)))
  case _: tl.Id =>
    args.foreach(lift(_, e))
    e.emit(inst(opcode.Mov, vm.Reg.Rpc, vm.Reg.Pc))
    e.emit(inst(opcode.Call, value.lift(lambda)))
  case lambda: tl.Lambda =>
    // e.emit(inst(opcode.PushReg, vm.Reg.Rpc))
    // e.emit(inst(opcode.PushReg, vm.Reg.Rst))
    args.foreach(lift(_, e))
    e.emit(inst(opcode.Mov, vm.Reg.Rpc, vm.Reg.Pc))
    e.emit(inst(opcode.Call, name(lambda.ptr)))
    lift(lambda, e.to(lambda.ptr))
  case app: tl.App =>
    call(app.lambda, app.args, e)
    e.emit(inst(opcode.Mov, vm.Reg.Jmp))
    args.foreach(lift(_, e))
    e.emit(inst(opcode.Mov, vm.Reg.Rpc, vm.Reg.Pc))
    e.emit(inst(opcode.Call))
    // println(app.lambda)
    // println(app.args)
    // e.emit(inst(opcode.Halt))
    // lift(app.lambda, e)
    // e.emit(inst(opcode.Halt))
  case _ =>
    /* bad call */
    ???

def load(label: String, e: Emitter) =
  e.emit(inst(opcode.LoadI32, name(label)))

def store(label: String, e: Emitter) =
  e.emit(inst(opcode.StoreI32, name(label)))

def storev(label: String, v: Ir, e: Emitter) = v match
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

def cond(cnd: Ir, pas: Ir, fal: Ir, e: Emitter) =
  val lcond = unique("cond")
  val lthen = unique("then")
  val lelse = unique("else")
  val ldone = unique("done")

  e.emit(inst(opcode.Label, lcond))
  lift(cnd, e)
  e.emit(inst(opcode.Jz, lelse))
  e.emit(inst(opcode.Label, lthen))
  lift(pas, e)
  e.emit(inst(opcode.Jmp, ldone))
  e.emit(inst(opcode.Label, lelse))
  lift(fal, e)
  e.emit(inst(opcode.Label, ldone))

def let(bindings: List[tl.Binding], body: Ir, e: Emitter) =
  bindings.foreach { case tl.Binding(ast.Id(label, _), value, _) =>
    lift(value, e)
    storev(label, value, e)
  }
  lift(body, e)

def lambda(params: List[tl.Id], body: Ir, e: Emitter) =
  params.reverse.foreach { case tl.Id(ast.Id(label, _)) =>
    store(label, e)
  }
  lift(body, e)
  e.emit(inst(opcode.Ret))
