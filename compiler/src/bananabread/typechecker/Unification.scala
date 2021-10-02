package bananabread
package typechecker
package unification


import ty._
import error._
import ir.linked

import utils.squished

import scala.collection.mutable.{Map => MutableMap}


type Attempt = Either[InferenceErr, _]
type Applied = Either[InferenceErr, Type]


object Substitution:
  def empty = Substitution()

class Substitution:
  private[this] val data = MutableMap.empty[Var, Type]

  private[unification] def has(v: Var) = data.contains(v)
  private[unification] def set(v: Var, ty: Type) = Right(data.update(v, ty))
  private[unification] def get(v: Var) = data(v)

  def unify(a: Type, b: Type, node: linked.Ir): Attempt =
    (a, b) match
      case (_, _) if a == b => Right(())
      case (v1: Var, v2: Var) => unifyVarVar(this, v1, v2, node)
      case (v: Var, ty) => set(v, ty)
      case (ty, v: Var) => set(v, ty)
      case (Void, _) => Right(())
      case (a: Lambda, b: Lambda) => unifyLambdaLambda(this, a, b, node)
      case _ => Left(UnificationErr(a, b, node))

  def apply(ty: Type, node: linked.Ir): Applied =
    ty match
      case v: Var if has(v) => Right(get(v))
      case v: Var => Left(UnunifiedTypeVarErr(v, node))
      case I32 => Right(I32)
      case Void => Right(Void)
      case Str => Right(Str)
      case ty: Lambda => applyLambda(this, ty, node)
      case _ => ???


/** TODO Current var applications aren't enough, need to be more like
  * bisquit's.
  */
def unifyVarVar(sub: Substitution, a: Var, b: Var, node: linked.Ir): Attempt =
  if sub.has(a)
  then sub.unify(b, sub.get(a), node)
  else if sub.has(b)
  then sub.unify(a, sub.get(b), node)
  else sub.set(a, b)

/** TODO Ensure arity match
  */
def unifyLambdaLambda(sub: Substitution, a: Lambda, b: Lambda, node: linked.Ir): Attempt =
  a.zip(b).map { (a, b) => sub.unify(a, b, node) }.squished

def applyLambda(sub: Substitution, lam: Lambda, node: linked.Ir): Applied =
  for
    in <- lam.in.map(sub(_, node)).squished
    out <- sub(lam.out, node)
  yield
    Lambda(in, out)
