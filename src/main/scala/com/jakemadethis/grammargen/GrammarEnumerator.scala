package com.jakemadethis.grammargen;
import scala.collection.mutable.{Map => MutableMap}
import scala.collection.mutable.Buffer
import scala.collection.mutable.ArrayBuffer

class GrammarEnumerator[SingleType, SeqType, F <: Form[SingleType, SeqType]]
  (val grammar: Grammar[SingleType, SeqType, F]) {
  
  import util._

  private def conv1(nt: SingleType) = convCache(nt).getN(1)
  private def lazyConv1(nt : SingleType)(x : BigInt) = conv1(nt)(x)

  def count(nt: SingleType, len : Int) = conv1(nt)(len)
  
  def countAll(set: MultiSet[SingleType], len : Int) : BigInt = {
    if (set.isEmpty) return d0(len)
    set.map { case (nt, num) => convCache(nt).getN(num) }
      .reduce { convolution(_,_) }(len)
  }
  
  def count(prod : F, len : Int) = countAll(prod.nonTerminalSet, len - prod.numTerminals)
  
  def countRange(derivation : F, min : Int, max : Int) = 
    (min to max).foldLeft(BigInt(0)) { case (result, i) => result + count(derivation, i) }
  
  def precompute(len : Int) {
    for (i <- 0 to len) {
      grammar.map.keys.foreach(conv1(_)(i))
    }

    for (nt <- grammar.map.keys) {
      convCache(nt).precompute(len)
    }
  }
  
  //

  
  private def sumSubtractN(fOrig: Func, f1: Func, t: BigInt)(n : BigInt) = {
    fOrig(n) + f1(n - t)
  }
  
  val convCache : Map[SingleType, Convolutor] = grammar.map.mapValues { prods =>

    val summation = prods.foldLeft(sumIdentity) { 
      case (sumResult, rule) => 

        // Todo: this should use the convFunc and multiset?
        val conv = 
          if (rule.rightSide.isTerminal) d0 
          else rule.rightSide.nonTerminals.map(lazyConv1 _).reduceLeft(convolution(_, _))
        
        sumSubtractN(sumResult, conv, rule.rightSide.numTerminals)
    }
    
    val func = makeMemo { x => if (x < 0) 0 else summation(x) }
    new MemoizableConvolutor(func)
  }.view.force


}

