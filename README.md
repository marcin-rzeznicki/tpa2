# tpa2

**Towarzystwo Przyjaciół Algorytmów - spotkanie 2**

Ćwiczeniem na to spotkanie jest implementacja algorytmu unifikacji dla prostego interpretera systemu Prologa. 
Proszę spróbować zaimplementować brakujące fragmenty [tu](/src/main/scala/org/virtuslab/tpa/prolog/Unification.scala)
oraz [tu](/src/main/scala/org/virtuslab/tpa/prolog/Substitution.scala), tak żeby testy przechodziły :-) 

Gramatyka języka jest mniej więcej taka:

```
<variable> ::= "#" <name>
<relational-term> ::= "[" <name> ": " <simple-term> <simple-terms> "]"
<simple-term> ::= <name> | <variable> | <relational-term>
<simple-terms> ::= "" | ", " <simple-term> <simple-terms>
<equality-assertion> ::= "<" <simple-term> " = " <simple-term> ">"
<non-equality-assertion> ::= "<" <simple-term> " /= " <simple-term> ">"
<complex-term> ::= <simple-term> | <equality-assertion> | <non-equality-assertion>
<complex-terms> ::= "" | <complex-term> <complex-terms-1>
<complex-terms-1> ::= "" | ", " <complex-term> <complex-terms-1>
<rule> ::= <simple-term> "." | "{(" <complex-terms> ") => " <simple-term> "}."
<query> ::= "(" <complex-terms> ")?"
<command> ::= "quit!"
<no-op> ::= "" | "%" <comment>
<op> ::= <rule> | <query> | <command> | <no-op>
<input-line> ::= <op> <EOL>
```

Można rzucić okiem na [testowe programy](/src/test/scala/org/virtuslab/tpa/prolog/programs).

Specyfikacja algorytmu unifikacji jest w [testach](/src/test/scala/org/virtuslab/tpa/prolog/UnificationTest.scala) :-),
ale dla porządku przeklejam formalny opis:
> 
    A name is equal to itself, but not equal to any other name or to any relational term.
    A relational term is equal to another relational term if they have the same name, the same number of sub-terms, and all sub-terms are pairwise equal. If any of the conditions do not hold, the two relational terms are not equal.
    A variable is always equal to itself. A variable may or may not be equal to another variable, but the conclusion cannot be made until both variables are (at least partially) assigned. Asserting equality of a variable to another term effectively assigns this variable.
    A variable may never be equal to a relational term involving the same variable.

(zwracam uwagę na ostatni punkt - on nie jest do końca oczywisty. Testy wychwycą :-)

Po napisaniu poprawnej implementacji możemy rozkoszować się interaktywną sesją 
(ewentualnie można w ten sposób sobie testować, jak ktoś lubi - parser na szczęście jest kompletny)

```
$ sbt
[info] Loading project definition from /home/rzeznik/IdeaProjects/TPA/prolog/project
[info] Set current project to prolog (in build file:/home/rzeznik/IdeaProjects/TPA/prolog/)
> run
[info] Running org.virtuslab.tpa.prolog.Session 

[r: a].
Ok.
{([r: #z]) => [p: [r: #z], #z]}.
Ok.
([p: [r: a], a])?
SAT
Ready.
([p: #a, a])?
SAT:
=====
 #a := [r: a]
Ready.
Bye.
[success] Total time: 139 s, completed Apr 20, 2016 1:45:02 AM
```
