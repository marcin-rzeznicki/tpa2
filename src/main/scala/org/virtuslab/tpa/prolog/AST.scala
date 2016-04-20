package org.virtuslab.tpa.prolog

object AST {
  sealed abstract class ComplexTerm {
    type RenamesTo <: ComplexTerm

    def vars: Set[Var]
    def ground: Boolean = vars.isEmpty
    def rename(f: String => String): RenamesTo
  }

  sealed abstract class Term extends ComplexTerm {
    override type RenamesTo <: Term

    def stringRep: String
    def show: String
    //helpful for testing
    def :-(body: ComplexTerm*) = Rule(this, body.toList)
    def ? = Query(List(this))
    def =:=(rhs: Term) = Eq(this, rhs)
    def =/=(rhs: Term) = Neq(this, rhs)
  }

  case class Var(name: String) extends Term {
    override type RenamesTo = Var

    override val vars: Set[Var] = Set(this)
    override val ground: Boolean = false

    override def stringRep: String = ""
    override def show: String = s"#$name"

    override def rename(f: (String) => String) = Var(f(name))
  }

  case class Atom(name: String) extends Term {
    override type RenamesTo = Atom

    override val vars: Set[Var] = Set.empty
    override val ground: Boolean = true

    override val stringRep: String = s"$name/0"
    override def show: String = name

    override def rename(f: (String) => String) = this
  }

  case class Rel(name: String, args: List[Term]) extends Term {
    override type RenamesTo = Rel

    import TermImplicits._

    override val vars: Set[Var] = args.freeVars

    val arity = args.length
    override val stringRep: String = s"$name/$arity"
    override def show: String = s"[$name: ${args.iterator map (_.show) mkString ", "}]"

    override def rename(f: (String) => String) = if (ground) this else this.copy(args = args map (_ rename f))
  }

  sealed abstract class Assertion extends ComplexTerm {
    override type RenamesTo <: Assertion

    def lhs: Term
    def rhs: Term

    override def vars: Set[Var] = lhs.vars ++ rhs.vars
    override val ground: Boolean = lhs.ground && rhs.ground
  }

  case class Eq(lhs: Term, rhs: Term) extends Assertion {
    override type RenamesTo = Eq

    override def rename(f: (String) => String) = if (ground) this else Eq(lhs rename f, rhs rename f)
  }

  case class Neq(lhs: Term, rhs: Term) extends Assertion {
    override type RenamesTo = Neq

    override def rename(f: (String) => String) = if (ground) this else Neq(lhs rename f, rhs rename f)
  }

  sealed trait Op

  sealed abstract class Clause extends Op {
    type RenamesTo <: Clause

    import TermImplicits._

    def head: Term
    def body: List[ComplexTerm]
    def freeVars: Set[Var] = head.vars ++ body.freeVars
    def renameVariables(f: String => String): RenamesTo
  }

  case class Fact(term: Term) extends Clause {
    override type RenamesTo = Fact

    override val head: Term = term
    override val body: List[ComplexTerm] = List.empty
    override def renameVariables(f: (String) => String) = if (term.ground) this else Fact(term rename f)
  }

  case class Rule(head: Term, body: List[ComplexTerm]) extends Clause {
    override type RenamesTo = Rule

    override def renameVariables(f: (String) => String) = Rule(head rename f, body mapConserve (_ rename f))
  }

  case class Query(terms: List[ComplexTerm]) extends Op

  case object Quit extends Op

  case object NoOp extends Op

}
