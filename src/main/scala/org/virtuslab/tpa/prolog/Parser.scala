package org.virtuslab.tpa.prolog

import scala.util.parsing.combinator.RegexParsers

object Parser extends RegexParsers {
  import AST._
  override val skipWhitespace: Boolean = false
  private val listSep = ", "
  private val nameRegex = """[a-zA-Z][a-zA-Z0-9-]*""".r
  private val commentRegex = """%.*""".r
  private val emptyRegex = "".r

  def name: Parser[String] = nameRegex
  def variable: Parser[Var] = elem('#') ~> name ^^ Var
  def simpleTerm: Parser[Term] = (name ^^ Atom) | variable | relationalTerm
  def simpleTerms: Parser[List[Term]] = rep1sep(simpleTerm, listSep)
  def relationalTerm: Parser[Rel] = (elem('[') ~> name <~ ": ") ~ (simpleTerms <~ elem(']')) ^^ {
    case name ~ terms => Rel(name, terms)
  }
  def equalityAssertion: Parser[Eq] = (elem('<') ~> simpleTerm <~ " = ") ~ (simpleTerm <~ elem('>')) ^^ {
    case term1 ~ term2 => Eq(term1, term2)
  }
  def nonEqualityAssertion: Parser[Neq] = (elem('<') ~> simpleTerm <~ " /= ") ~ (simpleTerm <~ elem('>')) ^^ {
    case term1 ~ term2 => Neq(term1, term2)
  }
  def complexTerm: Parser[ComplexTerm] = simpleTerm | equalityAssertion | nonEqualityAssertion
  def complexTerms: Parser[List[ComplexTerm]] = repsep(complexTerm, listSep)
  def rule: Parser[Clause] = (simpleTerm <~ elem('.') ^^ Fact) |
    (("{(" ~> complexTerms <~ ") => ") ~ (simpleTerm <~ "}.") ^^ {
      case terms ~ term => Rule(term, terms)
    })
  def query: Parser[Query] = elem('(') ~> complexTerms <~ ")?" ^^ Query
  def command: Parser[Op] = "quit!" ^^^ Quit
  def noOp: Parser[Op] = (commentRegex | emptyRegex) ^^^ NoOp
  def op: Parser[Op] = rule | query | command | noOp

  def parseLine(line: CharSequence) = parseAll(op, line)
}
