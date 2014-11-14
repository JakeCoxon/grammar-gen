package com.jakemadethis.grammargen.generators

import com.jakemadethis.grammargen.Form
import com.jakemadethis.collections.{LazyList, LazyListNil}

trait DerivationGenerator[SingleType, SeqType, Self <: DerivationGenerator[SingleType, SeqType, Self]] {
  this: Self =>
  def derivations: LazyList[Self]
  def result : SeqType
  val initial : Form[SingleType, SeqType]

  def allDerivations : Stream[Self] = {
    def derivationsOfList(list : LazyList[Self]) : Stream[Self] = {
      list.headOption match {
        case Some(head) => head #:: head.allDerivations #::: derivationsOfList(list.tail)
        case _ => Stream.empty
      }
    }
    derivationsOfList(derivations)
  }
}