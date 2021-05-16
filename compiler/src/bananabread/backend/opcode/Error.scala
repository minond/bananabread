package bananabread
package backend.opcode

import ir.typeless.{Ir, Id}
import parsing.ast.Str
import parsing.error.SyntaxErr
import parsing.opcode.Expr => OpcodeExpr
import runtime.instruction.Type


sealed trait GeneratorError
case class BadPushErr(ty: Type, node: Ir) extends GeneratorError
case class BadCallErr(lambda: Ir) extends GeneratorError
case class OpcodeSyntaxErr(source: Str, err: SyntaxErr) extends GeneratorError
case class UndeclaredIdentifierErr(id: Id) extends GeneratorError
case class CannotStoreDefErr(definition: Ir) extends GeneratorError
case class UnknownUserOpcodeErr(expr: OpcodeExpr) extends GeneratorError
case class InvalidI32Err(expr: OpcodeExpr) extends GeneratorError
