package com.jakemadethis.grammargen;

import com.jakemadethis.collections.MultiSet

object Form {
  def apply[InSingleType, InSeqType, SingleType, SeqType]
    (seq: InSeqType)(implicit formGen : FormConverter[InSingleType, InSeqType, SingleType, SeqType]) =
      formGen.createForm(seq)
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


trait FormGenerator[SeqType] {
  def createForm(thing: SeqType) : Form[_,SeqType]
  def deriveThing(head : SeqType, tail : SeqType) : SeqType
}
trait FormConverter[InSingleType, InSeqType, SingleType, SeqType] {
  def createNonTerminal(thing : InSingleType) : SingleType
  def createForm(thing: InSeqType) : Form[SingleType, SeqType]
}