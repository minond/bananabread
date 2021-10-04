package bananabread
package program

import parsing.ast
import ir.typed


case class SourceFile(module: Option[ast.Module], imports: List[ast.Import], tree: ast.Tree)
case class Name(value: String)


case class Module(name: Name, ir: List[typed.Ir]):
  def get(name: String): Option[typed.Ir] =
    ir.find {
      case typed.Def(ast.Id(name2, _), _, _, _) if name == name2 => true
      case _ => false
    }


type ModuleSpace = Map[Name, Module]

extension (space: ModuleSpace)
  def search(maybeSource: Option[ast.Import], name: String): Option[typed.Ir] =
    maybeSource match
      case None => None
      case Some(stmt) => search(stmt, name)
  def search(stmt: ast.Import, name: String): Option[typed.Ir] =
    search(stmt.name.id.lexeme, name)
  def search(source: String, name: String): Option[typed.Ir] =
    space.get(Name(source)) match
      case None => None
      case Some(mod) => mod.get(name)

object ModuleSpace:
  def empty: ModuleSpace =
    Map.empty
  def from(name: String, ir: List[typed.Ir]): ModuleSpace =
    from(Name(name), ir)
  def from(name: Name, ir: List[typed.Ir]): ModuleSpace =
    Map(name -> Module(name, ir))
