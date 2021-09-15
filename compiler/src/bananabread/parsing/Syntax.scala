package bananabread
package parsing

import ast.{Token, Id}


sealed trait OpPosition
case object Prefix extends OpPosition
case object Infix extends OpPosition
case object Postfix extends OpPosition


case class Syntax(
  prefix: Map[String, Int] = Map.empty,
  infix: Map[String, Int] = Map.empty,
  postfix: Map[String, Int] = Map.empty,
):
  def isOp(id: Id) = isPrefix(id) || isInfix(id) || isPostfix(id)
  def isOp(lexeme: String) = isPrefix(lexeme) || isInfix(lexeme) || isPostfix(lexeme)
  def isPrefix(id: Id) = prefix.keySet.contains(id.lexeme)
  def isPrefix(lexeme: String) = prefix.keySet.contains(lexeme)
  def isInfix(id: Id) = infix.keySet.contains(id.lexeme)
  def isInfix(lexeme: String) = infix.keySet.contains(lexeme)
  def isPostfix(id: Id) = postfix.keySet.contains(id.lexeme)
  def isPostfix(lexeme: String) = postfix.keySet.contains(lexeme)
  def prefixPrecedence(id: Id) = postfix.get(id.lexeme).get
  def prefixPrecedence(lexeme: String) = postfix.get(lexeme).get
  def infixPrecedence(id: Id) = infix.get(id.lexeme).get
  def infixPrecedence(lexeme: String) = infix.get(lexeme).get
  def postfixPrecedence(id: Id) = postfix.get(id.lexeme).get
  def postfixPrecedence(lexeme: String) = postfix.get(lexeme).get
  def withPrefix(precedence: Int, lexeme: String) = Syntax(prefix + (lexeme -> precedence), infix, postfix)
  def withPrefix(precedence: Int, id: Id) = Syntax(prefix + (id.lexeme -> precedence), infix, postfix)
  def withInfix(precedence: Int, lexeme: String) = Syntax(prefix, infix + (lexeme -> precedence), postfix)
  def withInfix(precedence: Int, id: Id) = Syntax(prefix, infix + (id.lexeme -> precedence), postfix)
  def withPostfix(precedence: Int, lexeme: String) = Syntax(prefix, infix, postfix + (lexeme -> precedence))
  def withPostfix(precedence: Int, id: Id) = Syntax(prefix, infix, postfix + (id.lexeme -> precedence))
  def withOp(pos: OpPosition, precedence: Int, id: Id): Syntax = withOp(pos, precedence, id.lexeme)
  def withOp(pos: OpPosition, precedence: Int, lexeme: String): Syntax = pos match
    case Prefix  => withPrefix(precedence, lexeme)
    case Infix   => withInfix(precedence, lexeme)
    case Postfix => withPostfix(precedence, lexeme)

object Syntax:
  def withPrefix(precedence: Int, lexeme: String) = Syntax().withPrefix(precedence, lexeme)
  def withPrefix(precedence: Int, id: Id) = Syntax().withPrefix(precedence, id)
  def withInfix(precedence: Int, lexeme: String) = Syntax().withPrefix(precedence, lexeme)
  def withInfix(precedence: Int, id: Id) = Syntax().withPrefix(precedence, id)
  def withPostfix(precedence: Int, lexeme: String) = Syntax().withPrefix(precedence, lexeme)
  def withPostfix(precedence: Int, id: Id) = Syntax().withPrefix(precedence, id)
