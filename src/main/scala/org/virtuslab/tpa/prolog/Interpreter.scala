package org.virtuslab.tpa.prolog

import scala.annotation.tailrec

class Interpreter private(facts: Map[String, Vector[AST.Clause]]) {
  import AST._
  import Interpreter._
  import Unification._

  def eval(op: Op): Interpreter = op match {
    case rule: Clause =>
      val updatedInterpreter = this updated rule
      println("Ok.")
      updatedInterpreter
    case query@Query(_) =>
      val results = this ?- query
      println(results.show)
      println("Ready.")
      this
    case _ => this
  }

  def ?-(query: Query): Answer = {
    import TermImplicits._
    val goals = query.terms
    val vars = goals.freeVars

    def withVarsSorted(subst: Substitution) = (subst | vars).toSeq sortBy (_._1.name)

    if (goals.isEmpty) Sat(Nil)
    else {
      val derivations = satisfy(goals, selectClauses(goals), Substitution identity vars)
      if (derivations.isEmpty) Unsat
      else if (vars.isEmpty) Sat(Nil) else Sat(derivations map withVarsSorted)
    }
  }

  def updated(clause: Clause): Interpreter = {
    val stringRep = clause.head.stringRep
    val updatedClauses = facts get stringRep match {
      case Some(clauses) => clauses :+ clause
      case None => Vector(clause)
    }
    new Interpreter(facts.updated(stringRep, updatedClauses))
  }

  private def selectClauses(goals: List[ComplexTerm]): Seq[Clause] = goals match {
    case (term: Term) :: _ => facts.getOrElse(term.stringRep, Nil)
    case _ => Nil
  }

  private val uniqueNumber = Iterator.from(1)

  private def renameVariables(clause: Clause): Clause = {
    val id = uniqueNumber.next()
    clause renameVariables (s => s"_$s$id")
  }

  private sealed trait MatchResult

  private case class Match(subgoals: List[ComplexTerm], bindings: Substitution,
                           untried: Seq[Clause]) extends MatchResult

  private case object NoMatch extends MatchResult

  private class ChoicePoint(val goalList: List[ComplexTerm], val bindings: Substitution,
                            val continuation: Seq[Clause])

  @tailrec
  private def satisfy(goalList: List[ComplexTerm], db: Seq[Clause], σ: Substitution,
                      btStack: List[ChoicePoint] = Nil,
                      derivations: Vector[Substitution] = Vector.empty): Vector[Substitution] = goalList match {
    case goal :: goals => satisfy(goal, db, σ) match {
      case Match(subgoals, bindings, cont) =>
        val newGoals = subgoals ::: goals
        val newDb = selectClauses(newGoals)
        val newStack = if (cont.isEmpty) btStack else new ChoicePoint(goalList, σ, cont) +: btStack
        satisfy(newGoals, newDb, bindings, newStack, derivations)
      case NoMatch => btStack match {
        case Nil => derivations
        case cp :: tl => satisfy(cp.goalList, cp.continuation, cp.bindings, tl, derivations)
      }
    }
    case Nil =>
      val newDerivations = derivations :+ σ
      btStack match {
        case Nil => newDerivations
        case cp :: tl => satisfy(cp.goalList, cp.continuation, cp.bindings, tl, newDerivations)
      }
  }

  private def satisfy(goal: ComplexTerm, db: Seq[Clause], σ: Substitution): MatchResult = goal match {
    case (t: Term) => findMatch(t, db, σ)
    case Eq(lhs, rhs) => unify(lhs, rhs, σ) match {
      case Unifiable(σ1) => Match(subgoals = Nil, bindings = σ1, untried = Nil)
      case ⊥ => NoMatch
    }
    case Neq(lhs, rhs) => unify(lhs, rhs, σ) match {
      case Unifiable(_) => NoMatch
      case ⊥ => Match(subgoals = Nil, bindings = σ, untried = Nil)
    }
  }

  @tailrec
  private def findMatch(goal: Term, db: Seq[Clause], σ: Substitution): MatchResult = if (db.isEmpty) NoMatch
  else {
    val selectedClause = renameVariables(db.head)
    val hd = selectedClause.head
    unify(goal, hd, σ ++ selectedClause.freeVars) match {
      case Unifiable(σ1) => Match(subgoals = selectedClause.body, bindings = σ1, untried = db.tail)
      case ⊥ => findMatch(goal, db.tail, σ)
    }
  }
}

object Interpreter {
  import AST._

  def apply() = new Interpreter(Map.empty)

  sealed trait Answer {
    def show: String
  }

  case class Sat(bindings: Seq[Seq[(Var, Term)]]) extends Answer {
    private val header =
      """SAT:
        |=====
        | """.stripMargin
    private val separator =
      """
        |SAT:
        |=====
        | """.stripMargin

    override def show: String = {
      def showBinding(binding: (Var, Term)) = s"${binding._1.show} := ${binding._2.show}"
      def showSeq(seq: Seq[(Var, Term)]) = seq.iterator map showBinding mkString "\n"

      if (bindings.isEmpty) "SAT" else bindings.iterator map showSeq mkString(start = header, sep = separator, end = "")
    }
  }

  case object Unsat extends Answer {
    override def show: String = "UNSAT"
  }
}
