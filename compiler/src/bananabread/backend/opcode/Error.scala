package bananabread
package backend.opcode.error

import ir.typeless.{Ir, Id}
import parsing.ast.Str
import parsing.error.SyntaxErr
import parsing.opcode.Expr => OpcodeExpr
import parsing.location.Location
import runtime.instruction.Type


sealed trait GeneratorErr
case class BadPushErr(ty: Type, node: Ir) extends GeneratorErr
case class BadCallErr(lambda: Ir) extends GeneratorErr
case class OpcodeSyntaxErr(source: Str, err: SyntaxErr) extends GeneratorErr
case class UndeclaredIdentifierErr(id: Id) extends GeneratorErr
case class CannotStoreDefErr(definition: Ir) extends GeneratorErr
case class UnknownUserOpcodeErr(expr: OpcodeExpr, source: String, loc: Location) extends GeneratorErr
case class InvalidI32Err(expr: OpcodeExpr) extends GeneratorErr
case class LookupErr(id: Id) extends GeneratorErr
