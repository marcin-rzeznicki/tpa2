package org.virtuslab.tpa.prolog

import scala.annotation.tailrec

object Unification {
  import AST._

  sealed trait UnificationResult

  case class Unifiable(unifier: Substitution) extends UnificationResult

  case object ⊥ extends UnificationResult

  def unify(term1: Term, term2: Term): UnificationResult = unify(term1, term2,
    Substitution identity (term1.vars ++ term2.vars))

  def unify(term1: Term, term2: Term, subst: Substitution): UnificationResult = {
    @tailrec
    def unifyAux(s: List[(Term, Term)], σ: Substitution): UnificationResult = if (s.isEmpty) Unifiable(σ)
    else {
      val (t1, t2) = s.head
      (t1 /: σ, t2 /: σ) match {
        case (Atom(x), Atom(y)) if x == y => unifyAux(s.tail, σ)
        case (Rel(f1, a1), Rel(f2, a2)) if f1 == f2 && a1.length == a2.length => unifyAux((a1 zip a2) ::: s.tail, σ)
        case (x@Var(_), any) if !σ.occursCheck(x, any) => unifyAux(s.tail, σ + (x -> any))
        case (any, y@Var(_)) if !σ.occursCheck(y, any) => unifyAux(s.tail, σ + (y -> any))
        case _ => ⊥
      }
    }

    unifyAux(List((term1, term2)), subst)
  }

}
