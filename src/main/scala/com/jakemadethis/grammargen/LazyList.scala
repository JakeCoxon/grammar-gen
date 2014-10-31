package com.jakemadethis.grammargen;

import scala.collection._
import scala.collection.mutable.ListBuffer
import scala.collection.generic._
import scala.collection.mutable.LazyBuilder
import scala.collection.mutable.Builder

object LazyList extends SeqFactory[LazyList] {  
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, () => A, LazyList[A]] = ???
       // new GenericCanBuildFrom[A]

  def newBuilder[A] = ???

  def listToLazyList[A](list : List[() => A]) : LazyList[A] = {
    if (list.isEmpty) return LazyListNil
    val tail_ = listToLazyList(list.tail)
    new LazyList[A] {
      val func = list.head
      override def tail = tail_
    }
  }

  def apply[A](seq : Seq[() => A]) : LazyList[A] = apply(seq.head, listToLazyList(seq.tail.toList))
  def apply[A](f: () => A, fs: () => A*) : LazyList[A] = apply(f, listToLazyList(fs.toList))


  def apply[A](f: () => A, tail_ : LazyList[A] = LazyListNil) : LazyList[A] = new LazyList[A] {
    val func = f
    override def tail = tail_
  }
}
 

case object LazyListNil extends LazyList[Nothing] {
  val func = null
  override def head = throw new NoSuchElementException("head of empty list")
  override def tail = throw new UnsupportedOperationException("tail of empty list")
  override def isEmpty = true
  override def length = 0
}
abstract class LazyList[+A] 
     extends Seq[A]
     with GenericTraversableTemplate[A, LazyList]
     with SeqLike[A, LazyList[A]] {
  
  val func : () => A
  lazy val value = func()

  override def companion = LazyList

  override def head : A = value
  override def isEmpty = false


  def iterator: Iterator[A] = ??? //new LazyListIterator(this)
  def apply(idx: Int): A = {
    if (idx == 0) value
    else if (idx < 0 || tail.isEmpty) throw new IndexOutOfBoundsException
    else tail(idx - 1)
  }
  def length: Int = 1 + tail.length
}

// class LazyListIterator private() extends AbstractIterator[A] with Iterator[A] {
//   var these : List
//   def this(self : LazyList[A]) {
//     this()
//     these = self
//   }
// }