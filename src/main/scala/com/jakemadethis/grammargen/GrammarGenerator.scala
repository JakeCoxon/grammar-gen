package com.jakemadethis.grammargen;

class GrammarGenerator[SingleType, SeqType, F <: Form[SingleType, SeqType]]
  (initial: F, grammar : Grammar[SingleType, SeqType, F]) {

  lazy val derivations = {
    val headNonTerminal = initial.nonTerminals.head 
    grammar(headNonTerminal).map { form =>
      val derived = initial.deriveForm(form.rightSide)
      new GrammarGenerator(derived, grammar)
    }
  }

  def result = initial.derivedResult

}



class BoundedGrammarGenerator[SingleType, SeqType, F <: Form[SingleType, SeqType]]
  (initial: F, enumerator : GrammarEnumerator[SingleType, SeqType, F])(size: Int) {
  val grammar = enumerator.grammar

  // def weighting(form: F, nt: SingleType, production : F, size: Int) = {
  //   val newSize = size - form.numTerminals
  //   val nts = form.nonTerminals
  //   enumerator.countAll(nts - nt + production.nonTerminals, 
  //       size - production.terminalSize)
  // }

  def derivations = {
    val headNonTerminal = initial.nonTerminals.head 
    grammar.map(headNonTerminal).map(_.rightSide)
  }
}
