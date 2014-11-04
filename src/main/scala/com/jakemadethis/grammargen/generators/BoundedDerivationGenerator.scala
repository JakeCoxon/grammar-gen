package com.jakemadethis.grammargen.generators

import com.jakemadethis.grammargen._
import com.jakemadethis.collections.LazyList

class BoundedDerivationGenerator[SingleType, SeqType]
  (initial: Form[SingleType, SeqType], enumerator : GrammarEnumerator[SingleType, SeqType])(size: Int)
  extends DerivationGenerator[SingleType, SeqType, BoundedDerivationGenerator[SingleType, SeqType]] {

  type FormType = Form[SingleType, SeqType]
  type ProdType = FormProductionRule[SingleType, SeqType]
  val grammar = enumerator.grammar

  // def weighting(form: F, nt: SingleType, production : F, size: Int) = {
  //   val newSize = size - form.numTerminals
  //   val nts = form.nonTerminals
  //   enumerator.countAll(nts - nt + production.nonTerminals, 
  //       size - production.terminalSize)
  // }

  private def makeLazy(t : (FormType, ProdType))() = t match { case (form, prod) =>
    new BoundedDerivationGenerator(form, enumerator)(size - prod.rightSide.numTerminals)
  }

  private def isFormValid(t : (FormType, ProdType)) = t match { case (form, prod) =>
    enumerator.countAll(form.nonTerminalSet, size - prod.rightSide.numTerminals) > 0
  }

  private def deriveForm(prod : FormProductionRule[SingleType, SeqType]) =
    (initial.deriveForm(prod.rightSide), prod)

  lazy val derivations : LazyList[BoundedDerivationGenerator[SingleType, SeqType]] =
    LazyList {
      initial.nonTerminals.headOption match {
        case Some(headNonTerminal) => grammar(headNonTerminal).map(deriveForm).filter(isFormValid).map(makeLazy)
        case None => Seq()
      }
    }
  
  def result = initial.derivedResult
}

