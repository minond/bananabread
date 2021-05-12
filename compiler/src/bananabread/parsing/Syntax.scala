package bananabread
package parsing.syntax

import parsing.ast.{Token, Id}


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

object Syntax:
  def withPrefix(precedence: Int, lexeme: String) = Syntax().withPrefix(precedence, lexeme)
  def withPrefix(precedence: Int, id: Id) = Syntax().withPrefix(precedence, id)
  def withInfix(precedence: Int, lexeme: String) = Syntax().withPrefix(precedence, lexeme)
  def withInfix(precedence: Int, id: Id) = Syntax().withPrefix(precedence, id)
  def withPostfix(precedence: Int, lexeme: String) = Syntax().withPrefix(precedence, lexeme)
  def withPostfix(precedence: Int, id: Id) = Syntax().withPrefix(precedence, id)


object Tokens:
  val COMMA = ','
  val DOT = '.'
  val OPENPAREN = '('
  val CLOSEPAREN = ')'
  val OPENCURLYPAREN = '{'
  val CLOSECURLYPAREN = '}'
  val OPENSQUAREBRAKET = '['
  val CLOSESQUAREBRAKET = ']'
  val FORWARDSLASH = '/'
  val PERCENTAGE = '%'
  val SINGLEQUOTE = '\''

  val all = Seq(
    COMMA,
    DOT,
    OPENPAREN,
    CLOSEPAREN,
    OPENCURLYPAREN,
    CLOSECURLYPAREN,
    OPENSQUAREBRAKET,
    CLOSESQUAREBRAKET,
  )

object Word:
  val FUNC = "func"
  val EQ = "="
  val IF = "if"
  val THEN = "then"
  val ELSE = "else"
  val LET = "let"
  val IN = "in"
  val BEGIN = "begin"
  val END = "end"
  val DEF = "def"

  def is(token: Token, word: String) = token match
    case id: Id => id.lexeme == word
    case _ => false

  def isFunc(token: Token) = is(token, FUNC)
  def isEq(token: Token) = is(token, EQ)
  def isIf(token: Token) = is(token, IF)
  def isLet(token: Token) = is(token, LET)
  def isIn(token: Token) = is(token, IN)
  def isBegin(token: Token) = is(token, BEGIN)
  def isEnd(token: Token) = is(token, END)
  def isDef(token: Token) = is(token, DEF)
