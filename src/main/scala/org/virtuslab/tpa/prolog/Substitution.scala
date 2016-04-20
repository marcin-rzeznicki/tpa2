package org.virtuslab.tpa.prolog

import org.virtuslab.tpa.prolog.AST.{Atom, Rel, Term, Var}

import scala.annotation.tailrec
import scala.collection.GenTraversableOnce

final class Substitution extends Equals {
  /**
    * Applies substitution e.g.
    * applying the substitution { x ↦ z, z ↦ h(a,y) } to the term f(z, a,g(x),y)
    * yields  f(h(a,y),a,g(z),y)
    *
    * @return instance of term t being the result of applying this substitution to term t
   */
  def apply(t: Term): Term = ???

  /**
    * Extends this substitution e.g.
    * { x ↦ 2, y ↦ 3+4 } + ( z ↦ a } yields { x ↦ 2, y ↦ 3+4, z ↦ a }
    *
    * @param binding new binding of variable to a term
    * @return substitution extended with binding
    */
  def +(binding: (Var, Term)): Substitution = ???

  /**
    * Extends this substitution with identity bindings of vars:
    * { x ↦ 2, y ↦ 3+4 } + ( a, b, c } is equal to { x ↦ 2, y ↦ 3+4 } + ( a ↦ a, b ↦ b, c ↦ c }
    * @param vars variables to be added
    * @return substitution extended with identity bindings of vars
    */
  def ++(vars: GenTraversableOnce[Var]): Substitution = ???

  /**
    *Restricts substitution to variables contained in vs e.g.
    * { x ↦ 2, y ↦ 3+4 } | {x } == Map(x -> 2)
    *
    * @param vs set of vars to restrict this substitution to
    * @return restricted substitution
    */
  def |(vs: Set[Var]): Map[Var, Term] = ???

  override def canEqual(that: Any): Boolean = that.isInstanceOf[Substitution]

  override def equals(a: Any): Boolean = ???

  override def hashCode(): Int = ???

}

object Substitution {
  /**
    *
    * @param mapping
    * @return substitution equivalent to mapping
    */
  def apply(mapping: Map[Var, Term]): Substitution = ???

  /**
    *
    * @param vars
    * @return identity substitution containing vars
    */
  def identity(vars: Set[Var]): Substitution = ???

  def identity(vars: Var*): Substitution = identity(vars.toSet)

  val empty = ???
  val ∅ = empty
}
