package org.virtuslab.tpa.prolog

import org.virtuslab.tpa.prolog.AST.{Atom, Rel, Term, Var}

import scala.annotation.tailrec
import scala.collection.GenTraversableOnce

final class Substitution private(val vSet: Set[Var], equivSet: UnionFind[Var], mapping: Map[Var, Term]) extends Equals {
  /**
    * Applies substitution e.g.
    * applying the substitution { x ↦ z, z ↦ h(a,y) } to the term f(z, a,g(x),y)
    * yields  f(h(a,y),a,g(z),y)
    *
    * @return instance of term t being the result of applying this substitution to term t
    */
  def apply(t: Term): Term = if (t.ground) t else substRec(t)

  def /:(t: Term): Term = if (t.ground || (t.vars forall free)) t else substRec(t)

  private def subst(t: Term): Term = t match {
    case Atom(_) => t
    case v@Var(_) => mapping.applyOrElse(equivSet(v), identity[Var])
    case r@Rel(_, as) => if (r.ground) r else r.copy(args = as map subst)
  }

  @tailrec
  private def substRec(t: Term): Term = {
    val t1 = subst(t)
    if (t1.ground || (t1.vars forall free)) t1 else substRec(t1)
  }

  def free(v: Var) = !mapping.isDefinedAt(equivSet(v))

  def occursCheck(v: Var, t: Term) = if (t.isInstanceOf[Rel]) t.vars exists (equivSet.equiv(_, v)) else false

  /**
    * Extends this substitution e.g.
    * { x ↦ 2, y ↦ 3+4 } + ( z ↦ a } yields { x ↦ 2, y ↦ 3+4, z ↦ a }
    *
    * @param binding new binding of variable to a term
    * @return substitution extended with binding
    */
  def +(binding: (Var, Term)): Substitution = {
    val (x, t) = binding
    if (x == t) this
    else {
      val (es, m) = bind(x, t)
      new Substitution(vSet, es, m)
    }
  }

  private def bind(x: Var, t: Term) = {
    assert(free(x), "free(x)")
    t match {
      case y@Var(_) if free(y) => (equivSet.union(y, x), mapping)
      case _ => (equivSet, mapping + (equivSet(x) -> t))
    }
  }

  /**
    * Extends this substitution with identity bindings of vars:
    * { x ↦ 2, y ↦ 3+4 } + ( a, b, c } is equal to { x ↦ 2, y ↦ 3+4 } + ( a ↦ a, b ↦ b, c ↦ c }
    *
    * @param vars variables to be added
    * @return substitution extended with identity bindings of vars
    */
  def ++(vars: GenTraversableOnce[Var]): Substitution = if (vars.isEmpty) this
  else new Substitution(vSet ++ vars, equivSet ++ vars, mapping)

  /**
    * Restricts substitution to variables contained in vs e.g.
    * { x ↦ 2, y ↦ 3+4 } | {x } == Map(x -> 2)
    *
    * @param vs set of vars to restrict this substitution to
    * @return restricted substitution
    */
  def |(vs: Set[Var]): Set[(Var, Term)] = vs map (v => v -> toMap(v))

  lazy val toMap: Map[Var, Term] = {
    val tautology: (Var, Term) => Boolean = _ == _
    vSet map (v => v -> this (v)) filterNot tautology.tupled toMap
  }

  override def canEqual(that: Any): Boolean = that.isInstanceOf[Substitution]

  override def equals(a: Any): Boolean = a match {
    case u: Substitution if this canEqual u => toMap == u.toMap
    case _ => false
  }

  override def hashCode(): Int = toMap.##

  override def toString = s"Substitution($toMap)"
}

object Substitution {
  import TermImplicits._
  /**
    *
    * @param mapping
    * @return substitution equivalent to mapping
    */
  def apply(mapping: Map[Var, Term]): Substitution = {
    val (renamings, substitutions) = mapping.partition {
      case (x, y@Var(_)) => true
      case (x, _) => false
    }.asInstanceOf[(Map[Var, Var], Map[Var, Term])]
    val nestedVars = substitutions.valuesIterator.freeVars

    val vSet = (mapping.keySet ++ nestedVars) ++ renamings.valuesIterator
    val equivSet = (UnionFind.makeSet(vSet) /: renamings) ((uf, v2) => uf.union(v2._2, v2._1))
    new Substitution(vSet, equivSet, substitutions)
  }

  /**
    *
    * @param vars
    * @return identity substitution containing vars
    */
  def identity(vars: Set[Var]): Substitution = if (vars.isEmpty) ∅ else new Substitution(vars, UnionFind.makeSet(vars), Map.empty)

  def identity(vars: Var*): Substitution = identity(vars.toSet)

  val empty = new Substitution(vSet = Set.empty, equivSet = UnionFind.empty, mapping = Map.empty)
  val ∅ = empty
}
