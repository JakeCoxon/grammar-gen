package com.jakemadethis.grammargen.generators

import com.jakemadethis.collections.LazyList

trait DerivationGenerator[SingleType, SeqType, Self <: DerivationGenerator[SingleType, SeqType, Self]] {
  this: Self =>
  def derivations: LazyList[Self]
  def result : SeqType
}