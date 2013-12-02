package com.jakemadethis.grammargen;

object MultiSet {
  def apply[K]() = new MultiSet[K]()
  def apply[K](seq: TraversableOnce[K]): MultiSet[K] = (apply[K] /: seq) { _ + _ }
  def apply[K](seq: (K, Int)*) = new MultiSet(Map(seq:_*))
  def apply[K](first: K, seq: K*): MultiSet[K] = apply(first +: seq)
}

class MultiSet[K](map: Map[K, Int] = Map[K, Int]()) extends Traversable[(K, Int)] {

  def add(value: K, count: Int) = updated(value, multiplicity(value) + count)
  def +(other: K) = add(other, 1)
  
  def +(other: Traversable[K]): MultiSet[K] = (this /: other) { _ + _ }
  def +(other: MultiSet[K]): MultiSet[K] = (this /: other) { (r, t) => r.add(t._1, t._2) }
  
  def -(other: K) = multiplicity(other) match {
    case 0 => this
    case 1 => new MultiSet(map - other)
    case m => updated(other, m - 1)
  }
  
  override def foreach[U](f: ((K,Int)) => U) = map.foreach(f)
  override def isEmpty = map.isEmpty
  
  def multiplicity(key: K): Int = map.getOrElse(key, 0)
  def contains(key: K) = map.contains(key)
  
  def updated(k: K, v: Int) = new MultiSet(map.updated(k, v))
  override def toString = map.toString
}