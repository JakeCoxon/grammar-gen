package com.jakemadethis.grammargen;

/*
 * A grammar is a set of ProductionRules and an initial form
 */
class Grammar[SingleType, SeqType]
  (val productions: Seq[FormProductionRule[SingleType,SeqType]]) {

  val map : Map[SingleType, Seq[FormProductionRule[SingleType,SeqType]]] = 
    productions.groupBy(k => k.leftSide)

  def apply(key : SingleType) = map(key)
  def get(key : SingleType) = map get key
}

object Form {
  def apply[InSingleType, InSeqType, SingleType, SeqType]
    (seq: InSeqType)(implicit formGen : FormConverter[InSingleType, InSeqType, SingleType, SeqType]) =
      formGen.createForm(seq)
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

trait FormGenerator[SeqType] {
  def createForm(thing: SeqType) : Form[_,SeqType]
  def deriveThing(head : SeqType, tail : SeqType) : SeqType
}
trait FormConverter[InSingleType, InSeqType, SingleType, SeqType] {
  def createNonTerminal(thing : InSingleType) : SingleType
  def createForm(thing: InSeqType) : Form[SingleType, SeqType]
}

/*
 * A Form is a state containing non-terminals and terminals. 
 * The form provides an ordering for non-terminals and only the number of terminals is needed
 */
case class Form[T,D](nonTerminals : Seq[T], numTerminals : Int, head : D, tail : Option[Form[T,D]])
  (implicit formGenerator : FormGenerator[D]) {

  def headOption = nonTerminals.headOption
  def isTerminal = nonTerminals.isEmpty
  lazy val nonTerminalSet = MultiSet(nonTerminals)
  lazy val isSingleton = nonTerminals.size == 1 && numTerminals == 0

  /*
   * Replaces the first non-terminal with replacedForm and returns the new Form
   * The following must be true
   * derivedForm.nonTerminals = replacedForm.nonTerminals ++ nonTerminals.tail
   * derivedForm.numTerminals = numTerminals + replacedForm.numTerminals
   */
  def deriveForm(replacedForm : Form[T,D]) =
    new Form(nonTerminals = replacedForm.nonTerminals ++ nonTerminals.tail,
                   numTerminals = numTerminals + replacedForm.numTerminals,
                   head = replacedForm.head, tail = Some(this))

  lazy val derivedResult : D = tail match {
    case None => head
    case Some(tail) => formGenerator.deriveThing(head, tail.derivedResult)
  }

  override def toString = s"Form(%s, %d, ?, ?)" format (nonTerminals, numTerminals)
}

case class ProductionRule[T,+D](leftSide: T, rightSide: D)
case class FormProductionRule[T,D](leftSide: T, rightSide: Form[T, D])