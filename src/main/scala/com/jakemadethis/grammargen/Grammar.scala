package com.jakemadethis.grammargen;

/*
 * A grammar is a set of ProductionRules and an initial form
 */
class Grammar[T, R <: ProductionRule[T]](val productions: Seq[R], val initialForm: Form[T]) {
  val map = productions.groupBy(_.leftSide)
  def apply(key : T) = map(key)
  def get(key : T) = map.get(key)
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


///////////////////////////////
// MultiSet
///////////////////////////////

object MultiSet {
  def apply[K]() = new MultiSet[K]()
  def apply[K](seq : TraversableOnce[K]) : MultiSet[K] = (apply[K] /: seq) { _ + _ }
  def apply[K](seq : (K, Int)*) = new MultiSet(Map(seq:_*))
  def apply[K](first: K, seq : K*) : MultiSet[K] = apply(first +: seq)
}
class MultiSet[K](map : Map[K, Int] = Map[K, Int]()) extends Traversable[(K, Int)] {

  def add(value : K, count : Int) = updated(value, multiplicity(value) + count)
  def +(other : K) = add(other, 1)
  
  def +(other : Traversable[K]) : MultiSet[K] = other.foldLeft(this) { (r, v) => r + v }
  def +(other : MultiSet[K]) : MultiSet[K] = other.foldLeft(this) { (r, t) => r.add(t._1, t._2) }
  
  def -(other : K) = multiplicity(other) match {
    case 0 => this
    case 1 => new MultiSet(map - other)
    case m => updated(other, m - 1)
  }
  
  override def foreach[U](f : ((K,Int)) => U) = map.foreach(f)
  override def isEmpty = map.isEmpty
  
  def multiplicity(key : K) : Int = map.getOrElse(key, 0)
  def contains(key : K) = map.contains(key)
  
  def updated(k : K, v : Int) = new MultiSet(map.updated(k, v))
  override def toString = map.toString
}
