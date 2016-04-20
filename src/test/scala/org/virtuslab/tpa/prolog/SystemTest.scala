package org.virtuslab.tpa.prolog

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}
import org.virtuslab.tpa.prolog.AST.Query
import org.virtuslab.tpa.prolog.Interpreter.Answer
import org.virtuslab.tpa.prolog.programs.{InsertionSort, MurderMystery, SimpleQueries}

class SystemTest extends PropSpec with TableDrivenPropertyChecks with Matchers {
  val sessions = Table(
    ("system", "query", "answer")
  ) ++ (SimpleQueries.testOutput ++ MurderMystery.testOutput ++ InsertionSort.testOutput).
    map(testOutput => (testOutput.programState, testOutput.query, testOutput.answer))

  property(
    "The goal of the system is to find all derivations and variable assignments such that the query holds true") {
    forAll(sessions) { (interpreter: Interpreter, query: Query, expectedAnswer: Answer) =>
      val answer = interpreter ?- query
      answer should be(expectedAnswer)
    }
  }
}
