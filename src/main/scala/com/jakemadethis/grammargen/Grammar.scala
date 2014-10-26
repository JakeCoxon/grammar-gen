package com.jakemadethis.grammargen;

/*
 * A grammar is a set of ProductionRules and an initial form
 */
class Grammar[SingleType, +SeqType, FormType <: Form[SingleType, SeqType]]
  (val productions: Seq[FormProductionRule[SingleType, FormType]], initialForm: FormType) {

  val map : Map[SingleType, Seq[FormProductionRule[SingleType, FormType]]] = 
    productions.groupBy(k => k.leftSide)

  def apply(key : SingleType) = map(key)
  def get(key : SingleType) = map get key
}

object Form {
  def apply[InSingleType, InSeqType, SingleType, SeqType, FormType <: Form[SingleType, SeqType]]
    (seq: InSeqType)(implicit formGen : FormConverter[InSingleType, InSeqType, SingleType, SeqType, FormType]) =
      formGen.createForm(seq)
}

class WrappedGrammar[InSingleType, InSeqType, SingleType, SeqType, FormType <: Form[SingleType, SeqType]]
    (productions: Seq[(InSingleType, InSeqType)], initial: InSeqType)
    (implicit formGen : FormConverter[InSingleType, InSeqType, SingleType, SeqType, FormType])
    extends Grammar[SingleType, SeqType, FormType](productions.map { rule =>
        FormProductionRule(formGen.createNonTerminal(rule._1), formGen.createForm(rule._2))
      },
      formGen.createForm(initial)) {
  def wrapKey(key : InSingleType) = super.apply(formGen.createNonTerminal(key))
  def getWrapKey(key : InSingleType) = super.get(formGen.createNonTerminal(key))
}

trait FormGenerator[SeqType, F <: Form[_, SeqType]] {
  def createForm(thing: SeqType) : F
}
trait FormConverter[InSingleType, InSeqType, SingleType, SeqType, F <: Form[SingleType, SeqType]] {
  def createNonTerminal(thing : InSingleType) : SingleType
  def createForm(thing: InSeqType) : F
}

/*
 * A Form is a state containing non-terminals and terminals. 
 * The form provides an ordering for non-terminals and only the number of terminals is needed
 */
abstract class Form[T,+D](val thing: D) {
  type FormType <: Form[T,D]
  val nonTerminals : Seq[T]
  val numTerminals : Int

  // type Path[R <: ProductionRule] = Seq[(T,R)]
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
  def deriveForm(replacedForm : FormType): FormType

  override def toString = 
    "Form(%s, %s, %d)" format (thing, nonTerminals.mkString(""), numTerminals)
}

case class ProductionRule[T,+D](leftSide: T, rightSide: D)
case class FormProductionRule[T,F <: Form[_,_]](leftSide: T, rightSide: F)