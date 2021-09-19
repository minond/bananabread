package bananabread
package backend.opcode

import parsing.ast
import ir.typed
import ir.typed.{Ir, Binding}

import scala.util.Random
import scala.collection.mutable.Map


object Scope:
  def empty =
    Scope("main", Map.empty, None)

class Scope(val module: String, env: Map[String, Ir], parent: Option[Scope]):
  def contains(id: ast.Id): Boolean = contains(id.lexeme)
  def contains(id: typed.Id): Boolean = contains(id.expr.lexeme)
  def contains(label: String): Boolean = (env.contains(label), parent) match
    case (true, _) => true
    case (_, Some(scope)) => scope.contains(label)
    case _ => false

  def get(id: ast.Id): Option[Ir] = get(id.lexeme)
  def get(id: typed.Id): Option[Ir] = get(id.expr.lexeme)
  def get(label: String): Option[Ir] = (env.get(label), parent) match
    case (Some(ir), _) => Some(ir)
    case (_, Some(scope)) => scope.get(label)
    case _ => None

  def qualified(id: ast.Id): String = qualified(id.lexeme)
  def qualified(id: typed.Id): String = qualified(id.expr.lexeme)
  def qualified(label: String): String = (env.get(label), parent) match
    case (Some(_), _) => s"$module.$label"
    case (_, Some(scope)) => scope.qualified(label)
    case _ => ???

  def qualified2(id: ast.Id): Option[String] = qualified2(id.lexeme)
  def qualified2(id: typed.Id): Option[String] = qualified2(id.expr.lexeme)
  def qualified2(label: String): Option[String] = (env.get(label), parent) match
    case (Some(_), _) => Some(s"$module.$label")
    case (_, Some(scope)) => scope.qualified2(label)
    case _ => None

  def define(bi: Binding): Unit = define(bi.label.lexeme, bi.value)
  def define(id: ast.Id, ir: Ir): Unit = define(id.lexeme, ir)
  def define(id: typed.Param, ir: Ir): Unit = define(id.name.lexeme, ir)
  def define(id: typed.Id, ir: Ir): Unit = define(id.expr.lexeme, ir)
  def define(label: String, ir: Ir): Unit =
    env.update(label, ir)

  def subscope(suffix: String) =
    Scope(s"$module.$suffix", Map.empty, Some(this))

  def forked[R](fn: Scope => R) =
    fn(Scope(module, Map.empty, Some(this)))
  def forked[R](module: String)(fn: Scope => R) =
    fn(Scope(module, Map.empty, Some(this)))

  def scoped[R](suffix: String)(fn: Scope => R) =
    fn(subscope(suffix))

  def unique[R](fn: Scope => R) =
    fn(subscope(Random.alphanumeric.take(4).mkString))

  def isToplevel: Boolean =
    parent.isEmpty
