package bisquit
package parser

import scala.util.{Try, Success, Failure}


// Main parser/lexer

def parse(sourceName: String, sourceString: String, syntax: Syntax): Either[ast.SyntaxErr, List[ast.Expr]] =
  tokenize(sourceName, sourceString, syntax).flatMap { tokens => parse(sourceName, tokens.iterator.buffered, syntax) }

def parse(sourceName: String, tokens: BufferedIterator[ast.Token], syntax: Syntax): Either[ast.SyntaxErr, List[ast.Expr]] =
  tokens
    .map { (token) => parseExpr(token, tokens, sourceName, syntax) }
    .squished

def parseExpr(head: ast.Token, tail: BufferedIterator[ast.Token], sourceName: String, syntax: Syntax): Either[ast.SyntaxErr, ast.Expr] =
  head match {
    case op: ast.Id if syntax.isPrefix(op) =>
      for rhs <- expectExpr(op, tail, sourceName, syntax)
      yield ast.Uniop(op, rhs)

    case lit: ast.Literal =>
      tail.headOption match {
        case Some(op: ast.Id) if syntax.isPostfix(op) =>
          Right(ast.Uniop(op, lit))

        case Some(op: ast.Id) if syntax.isInfix(op) =>
          for rhs <- expectExpr(op, skip(tail), sourceName, syntax)
          yield ast.Binop(lit, op, rhs)

        case Some(_) => Right(lit)
        case None => Right(lit)
      }
  }

def expectExpr(head: ast.Token, tail: BufferedIterator[ast.Token], sourceName: String, syntax: Syntax): Either[ast.SyntaxErr, ast.Expr] =
  tail.headOption match {
    case Some(_) =>
      parseExpr(tail.next, tail, sourceName, syntax)

    case None =>
      Left(ast.UnexpectedEofErr(head))
  }

def tokenize(sourceName: String, sourceString: String, syntax: Syntax): Either[ast.SyntaxErr, List[ast.Token]] =
  tokenize(sourceName, sourceString.iterator.zipWithIndex.buffered, syntax)

def tokenize(sourceName: String, sourceStream: BufferedIterator[(Char, Int)], syntax: Syntax): Either[ast.SyntaxErr, List[ast.Token]] =
  sourceStream
    .filter { (c, _) => !c.isWhitespace }
    .map { (c, i) => nextToken(c, sourceStream, ast.Location(sourceName, i), syntax) }
    .squished

def nextToken(head: Char, tail: BufferedIterator[(Char, Int)], loc: ast.Location, syntax: Syntax): Either[ast.SyntaxErr, ast.Token] =
  head match {
    case ',' => Right(ast.Comma(loc))
    case '.' => Right(ast.Dot(loc))
    case '(' => Right(ast.OpenParen(loc))
    case ')' => Right(ast.CloseParen(loc))
    case '{' => Right(ast.OpenCurlyParen(loc))
    case '}' => Right(ast.CloseCurlyParen(loc))
    case '[' => Right(ast.OpenSquareBraket(loc))
    case ']' => Right(ast.CloseSquareBraket(loc))

    case head if isNumHead(head) =>
      val rest = takeWhile(tail, isNumTail).mkString
      val lexeme = head +: rest

      Try { lexeme.toFloat } match {
        case Failure(_) => Left(ast.BadNumErr(lexeme, loc))
        case Success(_) => Right(ast.Num(lexeme, loc))
      }

    case head if isIdHead(head) =>
      val rest = takeWhile(tail, isIdTail).mkString
      val lexeme = head +: rest

      Right(ast.Id(lexeme, loc))

    case head =>
      val rest = takeWhile(tail, and(not(isIdTail), not(isWhitespace))).mkString
      val lexeme = head +: rest

      Right(ast.Id(lexeme, loc))
  }


// Syntax extensions

case class Syntax(prefix: Seq[String] = Seq.empty, infix: Seq[String] = Seq.empty, postfix: Seq[String] = Seq.empty) {
  def isOp(id: ast.Id) = isPrefix(id) || isInfix(id) || isPostfix(id)
  def isOp(lexeme: String) = isPrefix(lexeme) || isInfix(lexeme) || isPostfix(lexeme)

  def isPrefix(id: ast.Id) = prefix.contains(id.lexeme)
  def isPrefix(lexeme: String) = prefix.contains(lexeme)
  def isInfix(id: ast.Id) = infix.contains(id.lexeme)
  def isInfix(lexeme: String) = infix.contains(lexeme)
  def isPostfix(id: ast.Id) = postfix.contains(id.lexeme)
  def isPostfix(lexeme: String) = postfix.contains(lexeme)

  def withPrefix(lexeme: String) = Syntax(lexeme +: prefix, infix, postfix)
  def withPrefix(id: ast.Id) = Syntax(id.lexeme +: prefix, infix, postfix)
  def withInfix(lexeme: String) = Syntax(prefix, lexeme +: infix, postfix)
  def withInfix(id: ast.Id) = Syntax(prefix, id.lexeme +: infix, postfix)
  def withPostfix(lexeme: String) = Syntax(prefix, infix, lexeme +: postfix)
  def withPostfix(id: ast.Id) = Syntax(prefix, infix, id.lexeme +: postfix)
}


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

implicit class Eithers[L, R](val eithers: Iterator[Either[L, R]]) {
  /** Converts an [[Iterator[Either[L, R]]]] into an [[Either[L, List[R]]]].
   */
  def squished: Either[L, List[R]] =
    eithers.foldLeft[Either[L, List[R]]](Right(List())) {
      (acc, x) =>
        acc.flatMap(xs => x.map(xs :+ _))
    }
}
