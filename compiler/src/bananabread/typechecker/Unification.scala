package bananabread
package typechecker
package unification


import ty._
import error._
import ir.typeless

import utils.squished

import scala.collection.mutable.{Map => MutableMap}


type Attempt = Either[InferenceErr, _]
type Applied = Either[InferenceErr, Type]


object Substitution:
  def empty = Substitution()

class Substitution:
  private[this] val data = MutableMap.empty[Var, Type]
  private[this] def ok: Attempt = Right(())
  private[this] def set(v: Var, ty: Type): Attempt =
    data.update(v, ty)
    ok

  def unify(a: Type, b: Type): Attempt =
    (a, b) match
      case (_, _) if a == b => ok
      case (v1: Var, v2: Var) => set(v1, v2)
      case (v: Var, ty) => set(v, ty)
      case (ty, v: Var) => set(v, ty)
      case (Void, _) => Right(Void)
      case (_, Void) => Right(Void)
      case (a: Lambda, b: Lambda) => unifyLambdaLambda(this, a, b)
      case _ => ???

  def apply(ty: Type, node: typeless.Ir): Applied =
    ty match
      case v: Var if data.contains(v) => Right(data(v))
      case v: Var => Left(UnunifiedTypeVarErr(v, node))
      case I32 => Right(I32)
      case Void => Right(Void)
      case Str => Right(Str)
      case ty: Lambda => applyLambda(this, ty, node)
      case _ => ???


/** TODO: Ensure arity match
  */
def unifyLambdaLambda(sub: Substitution, a: Lambda, b: Lambda): Attempt =
  a.zip(b).map { (a, b) => sub.unify(a, b) }.squished

def applyLambda(sub: Substitution, lam: Lambda, node: typeless.Ir): Applied =
  for
    in <- lam.in.map(sub(_, node)).squished
    out <- sub(lam.out, node)
  yield
    Lambda(in, out)
