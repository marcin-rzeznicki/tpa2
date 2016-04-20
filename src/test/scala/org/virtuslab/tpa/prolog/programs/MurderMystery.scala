package org.virtuslab.tpa.prolog.programs

import org.virtuslab.tpa.prolog.AST._
import org.virtuslab.tpa.prolog.Interpreter.{Sat, Unsat}

object MurderMystery extends ProgramTestHelper {
  override val program =
    """% WatLog test case: Mystery (Simple)
      |% Col. Travis was poisoned...
      |
      |{([means: #x], [motive: #x], [opportunity: #x]) => [suspect: #x]}.
      |
      |{([doctor: #x], [poisoned: #y]) => [means: #x]}.
      |{([nurse: #x], [poisoned: #y]) => [means: #x]}.
      |{([veterinarian: #x], [poisoned: #y]) => [means: #x]}.
      |{([owns-firearm: #x], [shot: #y]) => [means: #x]}.
      |{([strong: #x], [strangled: #y]) => [means: #x]}.
      |
      |{([jealous-of: #x, #y], [victim: #y]) => [motive: #x]}.
      |{([inherits-from: #x, #y], [financial-trouble: #x], [victim: #y]) => [motive: #x]}.
      |
      |{([witnessed-by-at-on: #x, #z, #s, #t], [crime-scene: #s], [time-of-death: #t]) => [opportunity: #x]}.
      |{([has-no-alibi-for: #x, #t], [time-of-death: #t]) => [opportunity: #x]}.
      |
      |{([poisoned: #x]) => [victim: #x]}.
      |{([shot: #x]) => [victim: #x]}.
      |{([strangled: #x]) => [victim: #x]}.
      |
      |{([loves: #x, #y], [loves: #z, #y]) => [jealous-of: #x, #z]}.
      |
      |{([son-of: #x, #y]) => [inherits-from: #x, #y]}.
      |{([daughter-of: #x, #y]) => [inherits-from: #x, #y]}.
      |{([spouse-of: #x, #y]) => [inherits-from: #x, #y]}.
      |
      |[crime-scene: CountryHouse].
      |[time-of-death: Wednesday].
      |%[time-of-death: Tuesday].
      |[poisoned: ColTravis].
      |%[shot: ColTravis].
      |
      |% What if the Colonel was poisoned on Tuesday?
      |% What if he was shot rather than poisoned?
      |
      |ColTravis.
      |[spouse-of: ColTravis, Martha].
      |[loves: ColTravis, Martha].
      |[soldier: ColTravis].
      |[doctor: ColTravis].
      |[owns-firearm: ColTravis].
      |[strong: ColTravis].
      |[witnessed-by-at-on: ColTravis, Martha, CountryHouse, Tuesday].
      |[witnessed-by-at-on: ColTravis, Martha, CountryHouse, Wednesday].
      |
      |Martha.
      |[spouse-of: Martha, ColTravis].
      |[nurse: Martha].
      |[has-no-alibi-for: Martha, Wednesday].
      |
      |Jeffrey.
      |[son-of: Jeffrey, ColTravis].
      |[son-of: Jeffrey, Martha].
      |[golden-youth: Jeffrey].
      |[owns-firearm: Jeffrey].
      |[strong: Jeffrey].
      |[financial-trouble: Jeffrey].
      |[witnessed-by-at-on: Jeffrey, Mordred, CountryHouse, Wednesday].
      |
      |Susan.
      |[daughter-of: Susan, ColTravis].
      |[daughter-of: Susan, Martha].
      |[doctor: Susan].
      |[witnessed-by-at-on: Susan, Martha, CountryHouse, Tuesday].
      |
      |Mordred.
      |[loves: Mordred, Martha].
      |[veterinarian: Mordred].
      |[witnessed-by-at-on: Mordred, Susan, CountryHouse, Tuesday].
      |[has-no-alibi-for: Mordred, Wednesday].
      |
      |% Is Jeffrey a suspect?
      |([suspect: Jeffrey])?
      |
      |% Is Susan a suspect?
      |([suspect: Susan])?
      |
      |% List all known suspects.
      |([suspect: #x])?
      |
      |% Curiously, Col. Travis is a suspect in his own murder...
      |% While suicides certainly do happen, it doesn't seem plausible in this case.
      |% What motive could Col. Travis possibly have to take his own life?
      |% Perhaps, good old Watson didn't model the situation too well?
      |
      |quit!""".stripMargin

  private val CountryHouse = Atom("CountryHouse")
  private val Wednesday = Atom("Wednesday")
  private val Tuesday = Atom("Tuesday")

  private val ColTravis = Atom("ColTravis")
  private val Martha = Atom("Martha")
  private val Jeffrey = Atom("Jeffrey")
  private val Susan = Atom("Susan")
  private val Mordred = Atom("Mordred")

  override val syntaxTree = List(NoOp,
    NoOp,
    NoOp,
    suspect(X) :-(means(X), motive(X), opportunity(X)),
    NoOp,
    means(X) :-(doctor(X), poisoned(Y)),
    means(X) :-(nurse(X), poisoned(Y)),
    means(X) :-(veterinarian(X), poisoned(Y)),
    means(X) :-(ownsFirearm(X), shot(Y)),
    means(X) :-(strong(X), strangled(Y)),
    NoOp,
    motive(X) :-(jealousOf(X, Y), victim(Y)),
    motive(X) :-(inheritsFrom(X, Y), financialTrouble(X), victim(Y)),
    NoOp,
    opportunity(X) :-(witnessedByAtOn(X, Z, S, T), crimeScene(S), timeOfDeath(T)),
    opportunity(X) :-(hasNoAlibiFor(X, T), timeOfDeath(T)),
    NoOp,
    victim(X) :- poisoned(X),
    victim(X) :- shot(X),
    victim(X) :- strangled(X),
    NoOp,
    jealousOf(X, Z) :-(loves(X, Y), loves(Z, Y)),
    NoOp,
    inheritsFrom(X, Y) :- sonOf(X, Y),
    inheritsFrom(X, Y) :- daughterOf(X, Y),
    inheritsFrom(X, Y) :- spouseOf(X, Y),
    NoOp,
    Fact(crimeScene(CountryHouse)),
    Fact(timeOfDeath(Wednesday)),
    NoOp,
    Fact(poisoned(ColTravis)),
    NoOp,
    NoOp,
    NoOp,
    NoOp,
    NoOp,
    Fact(ColTravis),
    Fact(spouseOf(ColTravis, Martha)),
    Fact(loves(ColTravis, Martha)),
    Fact(soldier(ColTravis)),
    Fact(doctor(ColTravis)),
    Fact(ownsFirearm(ColTravis)),
    Fact(strong(ColTravis)),
    Fact(witnessedByAtOn(ColTravis, Martha, CountryHouse, Tuesday)),
    Fact(witnessedByAtOn(ColTravis, Martha, CountryHouse, Wednesday)),
    NoOp,
    Fact(Martha),
    Fact(spouseOf(Martha, ColTravis)),
    Fact(nurse(Martha)),
    Fact(hasNoAlibiFor(Martha, Wednesday)),
    NoOp,
    Fact(Jeffrey),
    Fact(sonOf(Jeffrey, ColTravis)),
    Fact(sonOf(Jeffrey, Martha)),
    Fact(goldenYouth(Jeffrey)),
    Fact(ownsFirearm(Jeffrey)),
    Fact(strong(Jeffrey)),
    Fact(financialTrouble(Jeffrey)),
    Fact(witnessedByAtOn(Jeffrey, Mordred, CountryHouse, Wednesday)),
    NoOp,
    Fact(Susan),
    Fact(daughterOf(Susan, ColTravis)),
    Fact(daughterOf(Susan, Martha)),
    Fact(doctor(Susan)),
    Fact(witnessedByAtOn(Susan, Martha, CountryHouse, Tuesday)),
    NoOp,
    Fact(Mordred),
    Fact(loves(Mordred, Martha)),
    Fact(veterinarian(Mordred)),
    Fact(witnessedByAtOn(Mordred, Susan, CountryHouse, Tuesday)),
    Fact(hasNoAlibiFor(Mordred, Wednesday)),
    NoOp,
    NoOp,
    suspect(Jeffrey) ?,
    NoOp,
    NoOp,
    suspect(Susan) ?,
    NoOp,
    NoOp,
    suspect(X) ?,
    NoOp,
    NoOp,
    NoOp,
    NoOp,
    NoOp,
    NoOp,
    Quit)

  override def testOutput: Seq[TestOutput] = Seq(
    TestOutput(stateAtTheEOF, suspect(Jeffrey) ?, Unsat),
    TestOutput(stateAtTheEOF, suspect(Susan) ?, Unsat),
    TestOutput(stateAtTheEOF, suspect(X) ?, Sat(Seq(Seq(X -> ColTravis), Seq(X -> Mordred))))
  )

  private def means(x: Term) = Rel("means", List(x))
  private def motive(x: Term) = Rel("motive", List(x))
  private def opportunity(x: Term) = Rel("opportunity", List(x))
  private def suspect(x: Term) = Rel("suspect", List(x))

  private def doctor(x: Term) = Rel("doctor", List(x))
  private def nurse(x: Term) = Rel("nurse", List(x))
  private def veterinarian(x: Term) = Rel("veterinarian", List(x))
  private def ownsFirearm(x: Term) = Rel("owns-firearm", List(x))
  private def strong(x: Term) = Rel("strong", List(x))
  private def soldier(x: Term) = Rel("soldier", List(x))

  private def poisoned(x: Term) = Rel("poisoned", List(x))
  private def shot(x: Term) = Rel("shot", List(x))
  private def strangled(x: Term) = Rel("strangled", List(x))

  private def jealousOf(x: Term, y: Term) = Rel("jealous-of", List(x, y))
  private def inheritsFrom(x: Term, y: Term) = Rel("inherits-from", List(x, y))
  private def witnessedByAtOn(x: Term, z: Term, s: Term, t: Term) = Rel("witnessed-by-at-on", List(x, z, s, t))
  private def hasNoAlibiFor(x: Term, t: Term) = Rel("has-no-alibi-for", List(x, t))

  private def victim(y: Term) = Rel("victim", List(y))
  private def financialTrouble(x: Term) = Rel("financial-trouble", List(x))
  private def crimeScene(s: Term) = Rel("crime-scene", List(s))
  private def timeOfDeath(t: Term) = Rel("time-of-death", List(t))

  private def loves(x: Term, y: Term) = Rel("loves", List(x, y))

  private def sonOf(x: Term, y: Term) = Rel("son-of", List(x, y))
  private def daughterOf(x: Term, y: Term) = Rel("daughter-of", List(x, y))
  private def spouseOf(x: Term, y: Term) = Rel("spouse-of", List(x, y))
  private def goldenYouth(x: Term) = Rel("golden-youth", List(x))
}
