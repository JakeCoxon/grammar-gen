package com.jakemadethis.grammargen;

/*
 * A grammar is a set of ProductionRules and an initial form
 */
class Grammar[SingleType, SeqType]
  (val productions: Seq[FormProductionRule[SingleType,SeqType]]) {

  private val map : Map[SingleType, Seq[FormProductionRule[SingleType,SeqType]]] = 
    productions.groupBy(k => k.leftSide)

  def apply(key : SingleType) = map(key)
  def get(key : SingleType) = map get key
  def toMap = map
}


class WrappedGrammar[InSingleType, InSeqType, SingleType, SeqType]
    (productions: Seq[(InSingleType, InSeqType)])
    (implicit formGen : FormConverter[InSingleType, InSeqType, SingleType, SeqType])
    extends Grammar[SingleType, SeqType](productions.map { rule =>
        FormProductionRule(formGen.createNonTerminal(rule._1), formGen.createForm(rule._2))
      }) {
  def wrapKey(key : InSingleType) = super.apply(formGen.createNonTerminal(key))
  def getWrapKey(key : InSingleType) = super.get(formGen.createNonTerminal(key))
}


case class ProductionRule[T,+D](leftSide: T, rightSide: D)
case class FormProductionRule[T,D](leftSide: T, rightSide: Form[T, D])