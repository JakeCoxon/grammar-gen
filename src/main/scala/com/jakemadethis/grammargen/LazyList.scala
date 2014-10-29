// package com.jakemadethis.grammargen;

// import scala.collection._
// import scala.collection.mutable.ListBuffer
// import scala.collection.generic._

// object LazyList extends SeqFactory[LazyList] {  
//   implicit def canBuildFrom[A]: CanBuildFrom[Coll, () => A, LazyList[A]] = ???
//        // new GenericCanBuildFrom[A]
//   def newBuilder[A] = ??? //new ListBuffer[() => A] mapResult (x => new LazyList(x:_*))
// }
 

// case object LazyListNil extends LazyList[Nothing] {
//   override def head = throw new NoSuchElementException("head of empty list")
//   override def tail = throw new UnsupportedOperationException("tail of empty list")
//   override def isEmpty = true
//   override def length = 0
// }
// class LazyList[A] 
//      extends Seq[A]
//      with GenericTraversableTemplate[A, LazyList]
//      with SeqLike[A, LazyList[A]] {
  
//   val func : () => A
//   lazy val value = func()

//   override def companion = LazyList

//   def head : A = value
//   def tail : LazyList[A]
//   def isEmpty = false


//   def iterator: Iterator[A] = ??? //new LazyListIterator(this)
//   def apply(idx: Int): A = {
//     if (idx == 0) value
//     else if (idx < 0 || tail == LazyListNil) throw new IndexOutOfBoundsException
//     else tail(idx - 1)
//   }
//   def length: Int = 1 + tail.length
// }

// // class LazyListIterator private() extends AbstractIterator[A] with Iterator[A] {
// //   var these : List
// //   def this(self : LazyList[A]) {
// //     this()
// //     these = self
// //   }
// // }