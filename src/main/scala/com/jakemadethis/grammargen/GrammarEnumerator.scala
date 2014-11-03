package com.jakemadethis.grammargen;
import com.jakemadethis.collections.MultiSet
import scala.collection.mutable.{Map => MutableMap}
import scala.collection.mutable.Buffer
import scala.collection.mutable.ArrayBuffer

class GrammarEnumerator[SingleType, SeqType]
  (val grammar: Grammar[SingleType, SeqType]) {
  
  import util._

  val convolutor : Convolutor[SingleType] = new MemoizableConvolutor(grammar)

  def count(nt: SingleType, len : Int) = convolutor.getN(nt)(1)(len)
  
  def countAll(set: MultiSet[SingleType], len : Int) : BigInt = {
    if (set.isEmpty) return d0(len)
    set.map { case (nt, num) => convolutor.getN(nt)(num) }
      .reduce { convolution(_,_) }(len)
  }
  
  def count(prod : Form[SingleType, SeqType], len : Int) = countAll(prod.nonTerminalSet, len - prod.numTerminals)
  
  def countRange(derivation : Form[SingleType, SeqType], min : Int, max : Int) = 
    (min to max).foldLeft(BigInt(0)) { case (result, i) => result + count(derivation, i) }
  
  def precompute(len : Int) = convolutor.precompute(len)
  

}

