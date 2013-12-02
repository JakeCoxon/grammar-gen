package com.jakemadethis.grammargen;

/*
 * A grammar is a set of ProductionRules and an initial form
 */
class Grammar[T, R <: ProductionRule[T]](val productions: Seq[R], val initialForm: Form[T]) {
  val map = productions.groupBy(_.leftSide)
  def apply(key : T) = map(key)
  def get(key : T) = map get key
  val nonTerminals = map.keys.toSet
}

/*
 * A Form is a state containing non-terminals and terminals. 
 * Non-terminals are stored in an ordered sequence while only the number of terminals are recorded.
 */
class Form[T](val nonTerminals: Seq[T], val numTerminals: Int) {
  
  // type Path[R <: ProductionRule] = Seq[(T,R)]
  def headOption = nonTerminals.headOption
  def isTerminal = nonTerminals.isEmpty
  val nonTerminalSet = MultiSet(nonTerminals)
  val isSingleton = nonTerminals.size == 1 && numTerminals == 0

  /*
   * Replaces the first non-terminal with replacedForm and returns the new Form
   */
  def deriveState(replacedForm : Form[T]) =
    new Form(replacedForm.nonTerminals ++ nonTerminals.tail, numTerminals + replacedForm.numTerminals)

  override def toString = 
    "Form(%s, %d)" format (nonTerminals.mkString(""), numTerminals)
}

class ProductionRule[T](val leftSide: T, val form: Form[T])
