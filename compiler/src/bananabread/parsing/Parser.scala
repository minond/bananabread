package bananabread
package parsing.parser

import parsing.ast

import ast.{Token, Tree, Expr, Stmt, SyntaxErr => Err}

import scala.reflect.ClassTag


type Tokens = BufferedIterator[Token]

// Parser predicates and combinators

type Pred[T] = T => Boolean
type Predcond = (Boolean, Boolean) => Boolean

def flpreds[T](preds: Seq[Pred[T]], id: Boolean = true)(cond: Predcond) =
  (c: T) => preds.foldLeft(id)((acc, pred) => cond(acc, pred(c)))

def ge[T <: Char](x: T) = (c: T) => c >= x
def le[T <: Char](x: T) = (c: T) => c <= x
def is[T <: Char](x: T) = (c: T) => c == x
def oneof[T <: Char](xs: T*) = (c: T) => xs.contains(c)
def not[T <: Char](f: Pred[T]) = flpreds(Seq(f))(_ && !_)
def and[T <: Char](fs: Pred[T]*) = flpreds(fs)(_ && _)
def or[T <: Char](fs: Pred[T]*) = flpreds(fs, false)(_ || _)

val isNewline = oneof('\r', '\n')
val isWhitespace = oneof(' ', '\t', '\r', '\n', '\f')
val isLetter = or(and(ge('a'), le('z')),
                  and(ge('A'), le('Z')))
val isNumeric = and(ge('0'),
                    le('9'))


// Predicate/combinator stream processors

def takeWhile[T](source: BufferedIterator[(T, _)], pred: Pred[T]): List[T] =
  def aux(buff: List[T]): List[T] =
    if source.isEmpty
    then buff
    else
      val curr = source.head
      if pred(curr._1)
      then aux(buff :+ source.next._1)
      else buff
  aux(List.empty)

def skip[T](it: BufferedIterator[T]): BufferedIterator[T] =
  it.next
  it

def eat[T: ClassTag](head: Token, tail: Tokens): Either[Err, T] =
  tail.headOption match
    case Some(token: T) =>
      tail.next
      Right(token)

    case Some(unexpected) => Left(ast.UnexpectedTokenErr[T](unexpected))
    case None => Left(ast.UnexpectedEofErr(head))

def eat(word: String, head: Token, tail: Tokens): Either[Err, Token] =
  tail.headOption match
    case Some(id: ast.Id) if Word.is(id, word) =>
      tail.next
      Right(id)

    case Some(unexpected) => Left(ast.UnexpectedTokenErr(unexpected))
    case None => Left(ast.UnexpectedEofErr(head))

def lookahead(head: Token, tail: Tokens): Token =
  tail.headOption match
    case None => ast.Eof(head.location)
    case Some(token) => token



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

  def is(token: ast.Token, word: String) = token match
    case id: ast.Id => id.lexeme == word
    case _ => false

  def isFunc(token: ast.Token) = is(token, FUNC)
  def isEq(token: ast.Token) = is(token, EQ)
  def isIf(token: ast.Token) = is(token, IF)
  def isLet(token: ast.Token) = is(token, LET)
  def isIn(token: ast.Token) = is(token, IN)
  def isBegin(token: ast.Token) = is(token, BEGIN)
  def isEnd(token: ast.Token) = is(token, END)
  def isDef(token: ast.Token) = is(token, DEF)
