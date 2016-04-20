package org.virtuslab.tpa.prolog

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.Tables.Table
import org.scalatest.{Matchers, PropSpec}
import org.virtuslab.tpa.prolog.AST.{Rel, Term}

class UnificationTest extends PropSpec with TableDrivenPropertyChecks with Matchers {
  import Unification._
  import UnificationTest._

  property("find the most general unifier") {
    forAll(unifications) { (term1: Term, term2: Term, solution: UnificationResult) =>
      unify(term1, term2) should be(solution)
    }
  }
}

object UnificationTest extends Helpers {

  import Substitution._
  import Unification._

  private def f(t: Term*) = Rel("f", t.toList)
  private def g(t: Term*) = Rel("g", t.toList)

  val unifications = Table(
    ("term1", "term2", "unifier"),
    (a, a, Unifiable(∅)),
    (a, b, ⊥),
    (X, X, Unifiable(identity(X))),
    (a, X, Unifiable(Substitution(Map(X -> a)))),
    (X, Y, Unifiable(Substitution(Map(X -> Y)))),
    (f(a, X), f(a, b), Unifiable(Substitution(Map(X -> b)))),
    (f(a), g(a), ⊥),
    (f(X), f(Y), Unifiable(Substitution(Map(X -> Y)))),
    (f(X), g(Y), ⊥),
    (f(X), f(Y, Z), ⊥),
    (f(g(X)), f(Y), Unifiable(Substitution(Map(Y -> g(X))))),
    (f(g(X), X), f(Y, a), Unifiable(Substitution(Map(X -> a, Y -> g(a))))),
    (X, f(X), ⊥),
    (f(f(f(f(a, Z), Y), X), W), f(W, f(X, f(Y, f(Z, a)))), Unifiable(Substitution(Map(
      Z -> a,
      Y -> f(a, a),
      X -> f(f(a, a), f(a, a)),
      W -> f(f(f(a, a), f(a, a)), f(f(a, a), f(a, a)))))))
  )
}
