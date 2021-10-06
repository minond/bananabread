package bananabread
package program

import parsing.ast
import loader.load
import ir.{typed, typeless, linked, stitched}
import error.Err


case class SourceFile(module: Option[ast.Module], imports: List[ast.Import], tree: ast.Tree)


case class ModDef(name: String)
object ModDef:
  def main = ModDef("main")
  def from(maybeSource: Option[ast.Ref]) = maybeSource match
    case None => main
    case Some(ref) => ModDef(ref.id.lexeme)
  def from(imp: ast.Import) = ModDef(imp.name.id.lexeme)
  def from(mod: ast.Module) = ModDef(mod.name.id.lexeme)

case class Module(defn: ModDef, ir: List[typed.Ir]):
  def get(name: String): Option[typed.Ir] =
    ir.find {
      case typed.Def(ast.Id(name2, _), _, _, _) if name == name2 => true
      case _ => false
    }


type ModuleSpace = Map[String, Module]

extension (space: ModuleSpace)
  def search(source: ModDef, name: String): Option[typed.Ir] =
    locate(source, name)._2

  def locate(source: ModDef, name: String): (Option[Module], Option[typed.Ir]) =
    space.get(source.name) match
      case None => (None, None)
      case Some(mod) => (Some(mod), mod.get(name))

object ModuleSpace:
  def empty: ModuleSpace =
    Map.empty
  def from(name: String, ir: List[typed.Ir]): ModuleSpace =
    Map(name -> Module(ModDef(name), ir))


def lift(source: SourceFile): Either[Err, List[stitched.Ir]] =
  for
    preludeCode <- load("Prelude")                 // XXX This needs to be moved
    preludeIr   <- lift0(preludeCode.head)         // XXX This needs to be moved
    space = ModuleSpace.from("Prelude", preludeIr) // XXX This needs to be moved

    ir0 <- typeless.lift(source.tree)
    ir1  = typeless.pass(ir0)
    ir2 <- linked.lift(ir1, source.module, source.imports)
    ir3 <- typed.lift(ir2, space)
    ir4 <- stitched.lift(ir3, space)
  yield
    ir4

def lift0(source: SourceFile): Either[Err, List[typed.Ir]] =
  for
    ir0 <- typeless.lift(source.tree)
    ir1 <- linked.lift(ir0, source.module, source.imports)
    ir2 <- typed.lift(ir1, ModuleSpace.empty)
  yield
    ir2

// def lift1(sources: List[SourceFile]): Either[Err, Program] =
//   val main = sources.head
//   val importedSources = main.imports.map { stmt => findSourceFile(stmt.name, sources) }.flatten
//
//   // if importedSources.isEmpty
//   // then lift(main)
//
//   val liftedImports =
//     if importedSources.isEmpty
//     then List.empty
//     else lift2(importedSources)
//
//   ???
