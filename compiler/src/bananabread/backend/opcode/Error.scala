package bananabread
package backend.opcode.error

import ir.stitched.{Ir, Id, Opcode}
import parsing.ast
import parsing.ast.Str
import parsing.error.SyntaxErr
import parsing.opcode.Expr => OpcodeExpr
import parsing.location.Location
import runtime.instruction.Type


sealed trait GeneratorErr
case class BadPushErr(ty: Type, node: Ir) extends GeneratorErr
case class BadCallErr(lambda: Ir) extends GeneratorErr
case class UndeclaredIdentifierErr(id: Id) extends GeneratorErr
case class CannotStoreErr(definition: Ir) extends GeneratorErr
case class UnknownUserOpcodeErr(expr: OpcodeExpr, loc: Location) extends GeneratorErr
case class InvalidI32Err(expr: OpcodeExpr) extends GeneratorErr
case class LookupErr(id: ast.Id) extends GeneratorErr
