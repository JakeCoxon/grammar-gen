package com.jakemadethis.grammargen;

/*
 * A grammar is a set of ProductionRules and an initial form
 */
class Grammar[T](val productions: Seq[ProductionRule[T]], val initialForm: Form[T]) {
  val map = productions.groupBy(k => hash(k.leftSide))
  def apply(key : T) = map(hash(key))
  def get(key : T) = map get hash(key)
  def hash(key: T) : Any = key
}

/*
 * A Form is a state containing non-terminals and terminals. 
 * The form provides an ordering for non-terminals and only the number of terminals is needed
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
