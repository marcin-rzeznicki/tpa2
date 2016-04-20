package org.virtuslab.tpa.prolog.programs

import org.virtuslab.tpa.prolog.AST.{Clause, ComplexTerm, Op, Query}
import org.virtuslab.tpa.prolog.Interpreter.Answer
import org.virtuslab.tpa.prolog.{Helpers, Interpreter}

trait ProgramTestHelper extends Helpers {
  def program: String
  def syntaxTree: List[Op]

  protected def populateFacts(ops: List[Op]): Interpreter = (Interpreter() /: ops) {
    case (facts, f: Clause) => facts updated f
    case (facts, _) => facts
  }

  protected def populateFactsFromSyntaxTree: Interpreter = populateFacts(syntaxTree)

  protected lazy val stateAtTheEOF = populateFactsFromSyntaxTree

  protected implicit class ListAsQuery(self: List[ComplexTerm]) {
    def ? = Query(self)
  }

  def testOutput: Seq[TestOutput] = Nil
}

case class TestOutput(programState: Interpreter, query: Query, answer: Answer)
