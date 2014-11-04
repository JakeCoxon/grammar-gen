package com.jakemadethis.grammargen.generators

import com.jakemadethis.grammargen._
import com.jakemadethis.collections.LazyList

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
