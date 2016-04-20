package org.virtuslab.tpa.prolog

import scala.annotation.tailrec

object Unification {
  import AST._

  sealed trait UnificationResult

  case class Unifiable(unifier: Substitution) extends UnificationResult

  case object ⊥ extends UnificationResult

  def unify(term1: Term, term2: Term): UnificationResult = unify(term1, term2,
    Substitution identity (term1.vars ++ term2.vars))

  def unify(term1: Term, term2: Term, subst: Substitution): UnificationResult = ???

}
