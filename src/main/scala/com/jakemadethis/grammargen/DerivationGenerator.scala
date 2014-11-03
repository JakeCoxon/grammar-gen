package com.jakemadethis.grammargen;
import com.jakemadethis.collections.LazyList

trait DerivationGenerator[SingleType, SeqType, Self <: DerivationGenerator[SingleType, SeqType, Self]] {
  this: Self =>
  def derivations: LazyList[Self]
  def result : SeqType
}

class UnboundedDerivationGenerator[SingleType, SeqType]
  (val initial: Form[SingleType, SeqType], val grammar : Grammar[SingleType, SeqType]) 
  extends DerivationGenerator[SingleType, SeqType, UnboundedDerivationGenerator[SingleType, SeqType]] {

  private def makeLazy(prod : FormProductionRule[SingleType, SeqType])() = {
    val derived = initial.deriveForm(prod.rightSide)
    new UnboundedDerivationGenerator(derived, grammar)
  }

  lazy val derivations : LazyList[UnboundedDerivationGenerator[SingleType, SeqType]] =
    LazyList {
      initial.nonTerminals.headOption match {
        case Some(headNonTerminal) => grammar(headNonTerminal).map(makeLazy)
        case None => Seq()
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

  private def makeLazy(prod : FormProductionRule[SingleType, SeqType])() = {
    val derived = initial.deriveForm(prod.rightSide)
    new BoundedDerivationGenerator(derived, enumerator)(size - prod.rightSide.numTerminals)
  }

  private def isProductionValid(prod : FormProductionRule[SingleType, SeqType]) = 
    enumerator.countAll(prod.rightSide.nonTerminalSet, size - prod.rightSide.numTerminals) > 0

  lazy val derivations : LazyList[BoundedDerivationGenerator[SingleType, SeqType]] =
    LazyList {
      initial.nonTerminals.headOption match {
        case Some(headNonTerminal) => grammar(headNonTerminal).filter(isProductionValid).map(makeLazy)
        case None => Seq()
      }
    }
  
  def result = initial.derivedResult
}
