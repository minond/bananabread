package bananabread
package program

import parsing.ast
import ir.typed


case class SourceFile(
  module: Option[ast.Module],
  imports: List[ast.Import],
  tree: ast.Tree
)


case class Program(main: Module, modules: Map[Name, Module])
case class Name(value: String)

case class Module(name: Name, imports: List[Import], ir: List[typed.Ir], stmt: Option[ast.Module])
object Module:
  def make(stmt: Option[ast.Module], imports: List[ast.Import], ir: List[typed.Ir]) =
    Module(
      Name(stmt.map(_.name.id.lexeme).getOrElse("main")),
      imports.map(Import.make),
      ir,
      stmt
    )

case class Import(name: Name, exposing: List[Name], stmt: ast.Import)
object Import:
  def make(stmt: ast.Import) =
    Import(
      Name(stmt.name.id.lexeme),
      stmt.exposing.map { node => Name(node.id.lexeme) },
      stmt
    )
