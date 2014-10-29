package com.jakemadethis.grammargen;

trait DerivationGenerator[SingleType, SeqType, Self <: DerivationGenerator[SingleType, SeqType, Self]] {
  this: Self =>
  def derivations: Traversable[Self]
  def result : SeqType
}

class InfiniteDerivationGenerator[SingleType, SeqType]
  (val initial: Form[SingleType, SeqType], val grammar : Grammar[SingleType, SeqType]) 
  extends DerivationGenerator[SingleType, SeqType, InfiniteDerivationGenerator[SingleType, SeqType]] {

  lazy val derivations = {
    val headNonTerminal = initial.nonTerminals.head 
    grammar(headNonTerminal).map { form =>
      val derived = initial.deriveForm(form.rightSide)
      new InfiniteDerivationGenerator(derived, grammar)
    }
  }

  def result = initial.derivedResult

}



class BoundedDerivationGenerator[SingleType, SeqType]
  (initial: Form[SingleType, SeqType], enumerator : GrammarEnumerator[SingleType, SeqType])(size: Int)
  extends DerivationGenerator[SingleType, SeqType, BoundedDerivationGenerator[SingleType, SeqType]] {
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
    ???
  }
  
  def result = initial.derivedResult
}
