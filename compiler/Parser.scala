package bananabread
package parser

import ast.{Token, Tree, Expr, Stmt, SyntaxErr => Err}
import utils.ListImplicits

import scala.util.{Try, Success, Failure}
import scala.reflect.ClassTag


type Tokens = BufferedIterator[Token]

// Main parser/lexer

def parse(sourceName: String, sourceString: String, syntax: Syntax): Either[Err, Tree] =
  tokenize(sourceName, sourceString, syntax).flatMap { tokens =>
    parse(sourceName, tokens.without[ast.Comment].iterator.buffered, syntax)
  }

def parse(sourceName: String, tokens: Tokens, syntax: Syntax): Either[Err, Tree] =
  for
    nodes <- tokens
      .map { (token) => parseExpr(token, tokens, sourceName, syntax) }
      .squished
  yield
    Tree(nodes)

def parseExpr(head: Token, tail: Tokens, sourceName: String, syntax: Syntax): Either[Err, Expr] =
  head match
    case op: ast.Id if syntax.isPrefix(op) => parseExprCont(parseUniop(op, tail, sourceName, syntax), tail, sourceName, syntax)
    case _ => parseExprCont(parsePrimary(head, tail, sourceName, syntax), tail, sourceName, syntax)

def parseUniop(op: ast.Id, tail: Tokens, sourceName: String, syntax: Syntax): Either[Err, ast.Uniop] =
  for rhs <- parsePrimary(tail.next, tail, sourceName, syntax)
  yield ast.Uniop(op, rhs)

def parsePrimary(head: Token, tail: Tokens, sourceName: String, syntax: Syntax): Either[Err, Expr] =
  head match
    case word: ast.Id if Word.isFunc(word) => parseLambda(word, tail, sourceName, syntax)
    case word: ast.Id if Word.isIf(word) => parseCond(word, tail, sourceName, syntax)
    case word: ast.Id if Word.isLet(word) => parseLet(word, tail, sourceName, syntax)
    case word: ast.Id if Word.isBegin(word) => parseBegin(word, tail, sourceName, syntax)
    case paren: ast.OpenParen => parseGroup(paren, tail, sourceName, syntax)
    case lit: ast.Num => Right(lit)
    case lit: ast.Str => Right(lit)
    case lit: ast.Id => Right(lit)
    case unexpected => Left(ast.UnexpectedTokenErr(unexpected))

def parseLambda(start: Token, tail: Tokens, sourceName: String, syntax: Syntax): Either[Err, ast.Lambda] =
  for
    args <- parseNextExprsByUntil[ast.Comma, ast.CloseParen](start, skip(tail), sourceName, syntax)
    eq <- eat(Word.EQ, start, tail)
    body <- parseNextExpr(eq, tail, sourceName, syntax)
  yield
    ast.Lambda(args, body)

def parseCond(start: Token, tail: Tokens, sourceName: String, syntax: Syntax): Either[Err, ast.Cond] =
  for
    cond <- parseExpr(tail.next, tail, sourceName, syntax)
    _ <- eat(Word.THEN, start, tail)
    pass <- parseExpr(tail.next, tail, sourceName, syntax)
    _ <- eat(Word.ELSE, start, tail)
    fail <- parseExpr(tail.next, tail, sourceName, syntax)
  yield
    ast.Cond(start, cond, pass, fail)

def parseLet(start: Token, tail: Tokens, sourceName: String, syntax: Syntax): Either[Err, ast.Let] =
  for
    bindings <- parseBindings(start, tail, sourceName, syntax)
    _ <- eat(Word.IN, start, tail)
    body <- parseExpr(tail.next, tail, sourceName, syntax)
  yield
    ast.Let(start, bindings, body)

def parseBindings(start: Token, tail: Tokens, sourceName: String, syntax: Syntax): Either[Err, List[ast.Binding]] =
  for
    binding <- parseBinding(start, tail, sourceName, syntax)
    next = lookahead(start, tail)
    bindings <- if Word.isIn(next)
                then Right(List.empty)
                else parseBindings(start, tail, sourceName, syntax)
  yield
    binding +: bindings

def parseBinding(start: Token, tail: Tokens, sourceName: String, syntax: Syntax): Either[Err, ast.Binding] =
  for
    label <- eat[ast.Id](start, tail)
    eq <- eat(Word.EQ, label, tail)
    value <- parseExpr(tail.next, tail, sourceName, syntax)
  yield
    ast.Binding(label, value)

def parseBegin(start: Token, tail: Tokens, sourceName: String, syntax: Syntax): Either[Err, ast.Begin] =
  for
    heade <- if Word.isEnd(lookahead(start, tail))
             then Left(ast.EmptyBeginNotAllowedErr(start))
             else parseExpr(tail.next, tail, sourceName, syntax)
    taile <- parseBeginTail(start, tail, sourceName, syntax)
    _ <- eat(Word.END, start, tail)
  yield
    ast.Begin(heade, taile)

def parseBeginTail(start: Token, tail: Tokens, sourceName: String, syntax: Syntax): Either[Err, List[Expr]] =
  for
    heade <- if Word.isEnd(lookahead(start, tail))
            then return Right(List.empty)
            else parseExpr(tail.next, tail, sourceName, syntax)
    next = lookahead(start, tail)
    taile <- if Word.isEnd(next)
            then Right(List.empty)
            else parseBeginTail(start, tail, sourceName, syntax)
  yield
    heade +: taile

def parseGroup(paren: Token, tail: Tokens, sourceName: String, syntax: Syntax): Either[Err, Expr] =
  for
    inner <- parseExpr(tail.next, tail, sourceName, syntax)
    _ <- eat[ast.CloseParen](paren, tail)
  yield
    inner

def parseExprCont(currRes: Either[Err, Expr], tail: Tokens, sourceName: String, syntax: Syntax): Either[Err, Expr] =
  currRes.flatMap { curr => parseExprCont(curr, tail, sourceName, syntax) }

def parseExprCont(curr: Expr, tail: Tokens, sourceName: String, syntax: Syntax): Either[Err, Expr] =
  tail.headOption match
    case Some(op: ast.Id) if syntax.isPostfix(op) =>
      tail.next
      Right(ast.Uniop(op, curr))

    case Some(op: ast.Id) if syntax.isInfix(op) =>
      for rhs <- parseNextExpr(op, skip(tail), sourceName, syntax)
      yield
        rhs match
          case ast.Binop(nextOp, nextLhs, nextRhs) if syntax.isInfix(nextOp) &&
            syntax.infixPrecedence(op) > syntax.infixPrecedence(nextOp) =>
              ast.Binop(nextOp, ast.Binop(op, curr, nextLhs), nextRhs)

          case _ => ast.Binop(op, curr, rhs)

    case Some(paren: ast.OpenParen) =>
      parseExprCont(parseNextExprsByUntil[ast.Comma, ast.CloseParen](paren, skip(tail), sourceName, syntax).map { args =>
        ast.App(curr, args)
      }, tail, sourceName, syntax)

    case Some(_) => Right(curr)
    case None => Right(curr)

def parseNextExprsByUntil[By: ClassTag, Until: ClassTag](
  head: Token,
  tail: Tokens,
  sourceName: String,
  syntax: Syntax,
  acc: List[Expr] = List.empty
): Either[Err, List[Expr]] =
  tail.headOption match
    case Some(_: Until) =>
      tail.next
      Right(acc)

    case Some(by: By) =>
      parseNextExpr(tail.next, tail, sourceName, syntax).flatMap { expr =>
        parseNextExprsByUntil[By, Until](head, tail, sourceName, syntax, acc :+ expr)
      }

    case Some(_) =>
      parseExpr(tail.next, tail, sourceName, syntax).flatMap { expr =>
        parseNextExprsByUntil[By, Until](head, tail, sourceName, syntax, acc :+ expr)
      }

    case None =>
      Left(ast.UnexpectedEofErr(head))

def parseNextExpr(head: Token, tail: Tokens, sourceName: String, syntax: Syntax): Either[Err, Expr] =
  tail.headOption match
    case Some(_) => parseExpr(tail.next, tail, sourceName, syntax)
    case None => Left(ast.UnexpectedEofErr(head))

def tokenize(sourceName: String, sourceString: String, syntax: Syntax): Either[Err, List[Token]] =
  tokenize(sourceName, sourceString.iterator.zipWithIndex.buffered, syntax)

def tokenize(sourceName: String, sourceStream: BufferedIterator[(Char, Int)], syntax: Syntax): Either[Err, List[Token]] =
  sourceStream
    .filter { (c, _) => !c.isWhitespace }
    .map { (c, i) => nextToken(c, sourceStream, ast.Location(sourceName, i), syntax) }
    .squished

def nextToken(head: Char, tail: BufferedIterator[(Char, Int)], loc: ast.Location, syntax: Syntax, ignoreComment: Boolean = false): Either[Err, Token] =
  head match
    case Tokens.COMMA => Right(ast.Comma(loc))
    case Tokens.DOT => Right(ast.Dot(loc))
    case Tokens.OPENPAREN => Right(ast.OpenParen(loc))
    case Tokens.CLOSEPAREN => Right(ast.CloseParen(loc))
    case Tokens.OPENCURLYPAREN => Right(ast.OpenCurlyParen(loc))
    case Tokens.CLOSECURLYPAREN => Right(ast.CloseCurlyParen(loc))
    case Tokens.OPENSQUAREBRAKET => Right(ast.OpenSquareBraket(loc))
    case Tokens.CLOSESQUAREBRAKET => Right(ast.CloseSquareBraket(loc))

    case Tokens.FORWARDSLASH if !ignoreComment =>
      tail.headOption match
        case Some(Tokens.FORWARDSLASH, _) =>
          val comment = takeWhile(skip(tail), not(isNewline))
          Right(ast.Comment(comment.mkString.strip, loc))
        case _ => nextToken(head, tail, loc, syntax, true)

    case head if isNumHead(head) =>
      val rest = takeWhile(tail, isNumTail).mkString
      val lexeme = head +: rest

      Try { lexeme.toFloat } match
        case Failure(_) => Left(ast.BadNumErr(lexeme, loc))
        case Success(_) => Right(ast.Num(lexeme, loc))

    case head if isIdHead(head) =>
      val rest = takeWhile(tail, isIdTail).mkString
      val lexeme = head +: rest

      Right(ast.Id(lexeme, loc))

    case head =>
      val rest = takeWhile(tail, isUnknownTail).mkString
      val lexeme = head +: rest

      Right(ast.Id(lexeme, loc))


// Syntax definition and extensions

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

case class Syntax(
  prefix: Map[String, Int] = Map.empty,
  infix: Map[String, Int] = Map.empty,
  postfix: Map[String, Int] = Map.empty,
):
  def isOp(id: ast.Id) = isPrefix(id) || isInfix(id) || isPostfix(id)
  def isOp(lexeme: String) = isPrefix(lexeme) || isInfix(lexeme) || isPostfix(lexeme)
  def isPrefix(id: ast.Id) = prefix.keySet.contains(id.lexeme)
  def isPrefix(lexeme: String) = prefix.keySet.contains(lexeme)
  def isInfix(id: ast.Id) = infix.keySet.contains(id.lexeme)
  def isInfix(lexeme: String) = infix.keySet.contains(lexeme)
  def isPostfix(id: ast.Id) = postfix.keySet.contains(id.lexeme)
  def isPostfix(lexeme: String) = postfix.keySet.contains(lexeme)
  def prefixPrecedence(id: ast.Id) = postfix.get(id.lexeme).get
  def prefixPrecedence(lexeme: String) = postfix.get(lexeme).get
  def infixPrecedence(id: ast.Id) = infix.get(id.lexeme).get
  def infixPrecedence(lexeme: String) = infix.get(lexeme).get
  def postfixPrecedence(id: ast.Id) = postfix.get(id.lexeme).get
  def postfixPrecedence(lexeme: String) = postfix.get(lexeme).get
  def withPrefix(precedence: Int, lexeme: String) = Syntax(prefix + (lexeme -> precedence), infix, postfix)
  def withPrefix(precedence: Int, id: ast.Id) = Syntax(prefix + (id.lexeme -> precedence), infix, postfix)
  def withInfix(precedence: Int, lexeme: String) = Syntax(prefix, infix + (lexeme -> precedence), postfix)
  def withInfix(precedence: Int, id: ast.Id) = Syntax(prefix, infix + (id.lexeme -> precedence), postfix)
  def withPostfix(precedence: Int, lexeme: String) = Syntax(prefix, infix, postfix + (lexeme -> precedence))
  def withPostfix(precedence: Int, id: ast.Id) = Syntax(prefix, infix, postfix + (id.lexeme -> precedence))

object Syntax:
  def withPrefix(precedence: Int, lexeme: String) = Syntax().withPrefix(precedence, lexeme)
  def withPrefix(precedence: Int, id: ast.Id) = Syntax().withPrefix(precedence, id)
  def withInfix(precedence: Int, lexeme: String) = Syntax().withPrefix(precedence, lexeme)
  def withInfix(precedence: Int, id: ast.Id) = Syntax().withPrefix(precedence, id)
  def withPostfix(precedence: Int, lexeme: String) = Syntax().withPrefix(precedence, lexeme)
  def withPostfix(precedence: Int, id: ast.Id) = Syntax().withPrefix(precedence, id)


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
val isNumHead = isNumeric
val isNumTail = or(isNumHead,
                   is('.'))
val isIdTail = and(not(isWhitespace),
                   or(isNumeric,
                      isLetter,
                      is('_')))
val isIdHead = and(isIdTail,
                   not(isNumeric))
val isUnknownTail = and(not(isIdTail),
                        not(isWhitespace),
                        not(oneof(Tokens.all:_*)))


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

implicit class Eithers[L, R](val eithers: Iterator[Either[L, R]]):
  /** Converts an [[Iterator[Either[L, R]]]] into an [[Either[L, List[R]]]].
   */
  def squished: Either[L, List[R]] =
    eithers.foldLeft[Either[L, List[R]]](Right(List())) {
      (acc, x) =>
        acc.flatMap(xs => x.map(xs :+ _))
    }
