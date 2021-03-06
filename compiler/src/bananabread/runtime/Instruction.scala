package bananabread
package runtime
package instruction

import typechecker.ty
import register.Register
import value.{I32, Id, Value}


sealed trait Type
case object I32 extends Type
case object Str extends Type
case object Reg extends Type
case object Ref extends Type
case object Ptr extends Type
case object Bool extends Type
case object Symbol extends Type
case object Lista extends Type
case object Scope extends Type
case object Const extends Type


/** TODO Find a corresponding runtime type for Void
  *
  * TODO Var should not be passed into this method. Once polymorphic types are
  * made concret the ty.Var match should go away.
  */
def toRuntimeType(typ: ty.Type): Type = typ match
  case ty.I32 => I32
  case ty.Str => Str
  case ty.Symbol => Symbol
  case ty.Bool => Bool
  case ty.Void => ???
  case ty.Lista => Ref
  case _: ty.Tuple => Ref
  case _: ty.Lambda => Ref
  case _: ty.Var => Ref


case class Label(label: String)
case class Data(typ: Type, label: String, value: Value)


sealed trait Instruction
case object Halt extends Instruction
case class Jz(label: String) extends Instruction
case class Jmp(label: String) extends Instruction
case class Push(typ: Type, value: Value) extends Instruction
case class Call(label: String) extends Instruction
case object Call0 extends Instruction
case object Ret extends Instruction
case object Swap extends Instruction
case class Mov(reg: Register, addr: Option[Id], offsetRaw: Option[I32], offsetReg: Option[Register]) extends Instruction
case class Stw(reg: Register) extends Instruction
case class Ldw(reg: Register) extends Instruction
case class Load(typ: Type, label: String) extends Instruction
case class Store(typ: Type, label: String) extends Instruction
case object Println extends Instruction
case object Concat extends Instruction
case class Add(typ: Type) extends Instruction
case class Sub(typ: Type) extends Instruction
case class Frame(argc: Int) extends Instruction
case class FrameInit(argc: Int) extends Instruction


type Code = Label
          | Data
          | Instruction

extension (codes: List[Code])
  def labels: Map[String, Int] =
    codes.zipWithIndex.collect {
      case (Label(label), index) => (label, index)
    }.toMap

  def constants: Map[String, Value] =
    codes.collect {
      case Data(_, label, value) => (label, value)
    }.toMap
