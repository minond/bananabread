package bananabread
package parsing.lang

import parsing.parser.{takeWhile, eat, skip, lookahead}
import parsing.parser.{and, is, not, oneof, or}
import parsing.parser.{isLetter, isNewline, isNumeric, isWhitespace}

import parsing.location.Location
import parsing.ast.{SyntaxErr => Err, _}
import parsing.syntax.{Syntax, Tokens, Word}

import utils.{ListImplicits, EitherImplicits, ComparisonImplicits}

import scala.util.{Try, Success, Failure}
import scala.reflect.ClassTag


type TokenBuffer = BufferedIterator[Token]

def parse(sourceName: String, sourceString: String, syntax: Syntax): Either[Err, Tree] =
  tokenize(sourceName, sourceString, syntax).flatMap { tokens =>
    parse(sourceName, tokens.without[Comment].iterator.buffered, syntax)
  }

def parse(sourceName: String, tokens: TokenBuffer, syntax: Syntax): Either[Err, Tree] =
  for
    nodes <- tokens
      .map { (token) => parseTop(token, tokens, sourceName, syntax) }
      .squished
  yield
    Tree(nodes)

def parseTop(head: Token, tail: TokenBuffer, sourceName: String, syntax: Syntax): Either[Err, Stmt | Expr] =
  head match
    case _ if Word.isDef(head) => parseDef(head, tail, sourceName, syntax)
    case _ => parseExpr(head, tail, sourceName, syntax)

def parseExpr(head: Token, tail: TokenBuffer, sourceName: String, syntax: Syntax): Either[Err, Expr] =
  head match
    case op: Id if syntax.isPrefix(op) => parseExprCont(parseUniop(op, tail, sourceName, syntax), tail, sourceName, syntax)
    case _ => parseExprCont(parsePrimary(head, tail, sourceName, syntax), tail, sourceName, syntax)

def parseUniop(op: Id, tail: TokenBuffer, sourceName: String, syntax: Syntax): Either[Err, Uniop] =
  for rhs <- parsePrimary(tail.next, tail, sourceName, syntax)
  yield Uniop(op, rhs)

def parsePrimary(head: Token, tail: TokenBuffer, sourceName: String, syntax: Syntax): Either[Err, Expr] =
  head match
    case word: Id if Word.isFunc(word) => parseLambda(word, tail, sourceName, syntax)
    case word: Id if Word.isIf(word) => parseCond(word, tail, sourceName, syntax)
    case word: Id if Word.isLet(word) => parseLet(word, tail, sourceName, syntax)
    case word: Id if Word.isBegin(word) => parseBegin(word, tail, sourceName, syntax)
    case paren: OpenParen => parseGroup(paren, tail, sourceName, syntax)
    case lit: Num => Right(lit)
    case lit: Str => Right(lit)
    case lit: Id => Right(lit)
    case lit: Symbol => Right(lit)
    case unexpected => Left(UnexpectedTokenErr(unexpected))

def parseLambda(start: Token, tail: TokenBuffer, sourceName: String, syntax: Syntax): Either[Err, Lambda] =
  for
    args <- parseNextExprsByUntil[Comma, CloseParen](start, skip(tail), sourceName, syntax)
    eq <- eat(Word.EQ, start, tail)
    body <- parseNextExpr(eq, tail, sourceName, syntax)
  yield
    Lambda(args, body)

def parseCond(start: Token, tail: TokenBuffer, sourceName: String, syntax: Syntax): Either[Err, Cond] =
  for
    cond <- parseExpr(tail.next, tail, sourceName, syntax)
    _ <- eat(Word.THEN, start, tail)
    pass <- parseExpr(tail.next, tail, sourceName, syntax)
    _ <- eat(Word.ELSE, start, tail)
    fail <- parseExpr(tail.next, tail, sourceName, syntax)
  yield
    Cond(start, cond, pass, fail)

def parseLet(start: Token, tail: TokenBuffer, sourceName: String, syntax: Syntax): Either[Err, Let] =
  for
    bindings <- parseBindings(start, tail, sourceName, syntax)
    _ <- eat(Word.IN, start, tail)
    body <- parseExpr(tail.next, tail, sourceName, syntax)
  yield
    Let(start, bindings, body)

def parseDef(start: Token, tail: TokenBuffer, sourceName: String, syntax: Syntax): Either[Err, Def] =
  for
    name <- eat[Id](start, tail)
    next = lookahead(start, tail)
    value <- if next.is[OpenParen]
             then parseLambda(start, tail, sourceName, syntax)
             else parseDefValue(start, tail, sourceName, syntax)
  yield
    Def(name, value)

def parseDefValue(start: Token, tail: TokenBuffer, sourceName: String, syntax: Syntax): Either[Err, Expr] =
  for
    _ <- eat(Word.EQ, start, tail)
    value <- parseExpr(tail.next, tail, sourceName, syntax)
  yield
    value

def parseBindings(start: Token, tail: TokenBuffer, sourceName: String, syntax: Syntax): Either[Err, List[Binding]] =
  for
    binding <- parseBinding(start, tail, sourceName, syntax)
    next = lookahead(start, tail)
    bindings <- if Word.isIn(next)
                then Right(List.empty)
                else parseBindings(start, tail, sourceName, syntax)
  yield
    binding +: bindings

def parseBinding(start: Token, tail: TokenBuffer, sourceName: String, syntax: Syntax): Either[Err, Binding] =
  for
    label <- eat[Id](start, tail)
    eq <- eat(Word.EQ, label, tail)
    value <- parseExpr(tail.next, tail, sourceName, syntax)
  yield
    Binding(label, value)

def parseBegin(start: Token, tail: TokenBuffer, sourceName: String, syntax: Syntax): Either[Err, Begin] =
  for
    heade <- if Word.isEnd(lookahead(start, tail))
             then Left(EmptyBeginNotAllowedErr(start))
             else parseExpr(tail.next, tail, sourceName, syntax)
    taile <- parseBeginTail(start, tail, sourceName, syntax)
    _ <- eat(Word.END, start, tail)
  yield
    Begin(heade, taile)

def parseBeginTail(start: Token, tail: TokenBuffer, sourceName: String, syntax: Syntax): Either[Err, List[Expr]] =
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

def parseGroup(paren: Token, tail: TokenBuffer, sourceName: String, syntax: Syntax): Either[Err, Expr] =
  for
    inner <- parseExpr(tail.next, tail, sourceName, syntax)
    _ <- eat[CloseParen](paren, tail)
  yield
    inner

def parseExprCont(currRes: Either[Err, Expr], tail: TokenBuffer, sourceName: String, syntax: Syntax): Either[Err, Expr] =
  currRes.flatMap { curr => parseExprCont(curr, tail, sourceName, syntax) }

def parseExprCont(curr: Expr, tail: TokenBuffer, sourceName: String, syntax: Syntax): Either[Err, Expr] =
  tail.headOption match
    case Some(op: Id) if syntax.isPostfix(op) =>
      tail.next
      Right(Uniop(op, curr))

    case Some(op: Id) if syntax.isInfix(op) =>
      for rhs <- parseNextExpr(op, skip(tail), sourceName, syntax)
      yield
        rhs match
          case Binop(nextOp, nextLhs, nextRhs) if syntax.isInfix(nextOp) &&
            syntax.infixPrecedence(op) > syntax.infixPrecedence(nextOp) =>
              Binop(nextOp, Binop(op, curr, nextLhs), nextRhs)

          case _ => Binop(op, curr, rhs)

    case Some(paren: OpenParen) =>
      parseExprCont(parseNextExprsByUntil[Comma, CloseParen](paren, skip(tail), sourceName, syntax).map { args =>
        App(curr, args)
      }, tail, sourceName, syntax)

    case Some(_) => Right(curr)
    case None => Right(curr)

def parseNextExprsByUntil[By: ClassTag, Until: ClassTag](
  head: Token,
  tail: TokenBuffer,
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
      Left(UnexpectedEofErr(head))

def parseNextExpr(head: Token, tail: TokenBuffer, sourceName: String, syntax: Syntax): Either[Err, Expr] =
  tail.headOption match
    case Some(_) => parseExpr(tail.next, tail, sourceName, syntax)
    case None => Left(UnexpectedEofErr(head))


def tokenize(sourceName: String, sourceString: String, syntax: Syntax): Either[Err, List[Token]] =
  tokenize(sourceName, sourceString.iterator.zipWithIndex.buffered, syntax)
def tokenize(sourceName: String, sourceStream: BufferedIterator[(Char, Int)], syntax: Syntax): Either[Err, List[Token]] =
  sourceStream
    .filter { (c, _) => !c.isWhitespace }
    .map { (c, i) => nextToken(c, sourceStream, Location(sourceName, i), syntax) }
    .squished

def nextToken(
  head: Char,
  tail: BufferedIterator[(Char, Int)],
  loc: Location,
  syntax: Syntax,
  ignoreComment: Boolean = false,
  ignorePString: Boolean = false,
): Either[Err, Token] = head match
  case Tokens.COMMA => Right(Comma(loc))
  case Tokens.DOT => Right(Dot(loc))
  case Tokens.OPENPAREN => Right(OpenParen(loc))
  case Tokens.CLOSEPAREN => Right(CloseParen(loc))
  case Tokens.OPENCURLYPAREN => Right(OpenCurlyParen(loc))
  case Tokens.CLOSECURLYPAREN => Right(CloseCurlyParen(loc))
  case Tokens.OPENSQUAREBRAKET => Right(OpenSquareBraket(loc))
  case Tokens.CLOSESQUAREBRAKET => Right(CloseSquareBraket(loc))

  case Tokens.FORWARDSLASH if !ignoreComment =>
    tail.headOption match
      case Some(Tokens.FORWARDSLASH, _) =>
        val comment = takeWhile(skip(tail), not(isNewline))
        Right(Comment(comment.mkString.strip, loc))
      case _ => nextToken(head, tail, loc, syntax, ignoreComment=true)

  case Tokens.PERCENTAGE if !ignorePString =>
    tail.headOption match
      case Some(Tokens.OPENCURLYPAREN, _) =>
        val str = takeWhile(skip(tail), not(is(Tokens.CLOSECURLYPAREN)))
        // TODO Unsafe head lookup
        if tail.next._1 != Tokens.CLOSECURLYPAREN
        then Left(UnclosedStringErr(loc))
        else Right(Str(str.mkString, loc))
      case _ => nextToken(head, tail, loc, syntax, ignorePString=true)

  case Tokens.SINGLEQUOTE =>
    val symbol = takeWhile(tail, isSymbolTail).mkString

    Right(Symbol(symbol, loc))

  case head if isNumHead(head) =>
    val rest = takeWhile(tail, isNumTail).mkString
    val lexeme = head +: rest

    Try { lexeme.toFloat } match
      case Failure(_) => Left(BadNumErr(lexeme, loc))
      case Success(_) => Right(Num(lexeme, loc))

  case head if isIdHead(head) =>
    val rest = takeWhile(tail, isIdTail).mkString
    val lexeme = head +: rest

    Right(Id(lexeme, loc))

  case head =>
    val rest = takeWhile(tail, isUnknownTail).mkString
    val lexeme = head +: rest

    Right(Id(lexeme, loc))


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
val isSymbolTail = and(not(isWhitespace),
                       not(oneof('(', ')', '{', '}', '[', ']')))
