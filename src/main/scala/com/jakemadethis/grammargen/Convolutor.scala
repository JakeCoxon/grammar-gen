package com.jakemadethis.grammargen;

import scala.collection.mutable.{Map => MutableMap}

trait Convolutor[SingleType] {
  def precompute(len : Int): Unit
  def getN(nonTerminal : SingleType)(n : Int): BigInt => BigInt
}

class MemoizableConvolutor[SingleType, SeqType](grammar : Grammar[SingleType, SeqType]) extends Convolutor[SingleType] {

  type SelfConvolutionCache = MutableMap[Int, BigInt => BigInt]
  import util._

  def getN(nonTerminal : SingleType)(n : Int) = convCache(nonTerminal).getOrElseUpdate(n, selfConvolution(nonTerminal, n))

  private def conv1(nt: SingleType) = convCache(nt)(1)
  private def lazyConv1(nt : SingleType)(x : BigInt) = conv1(nt)(x)

  private def selfConvolution(nonTerminal : SingleType, i : Int) = 
    makeMemo { x => convolution(convCache(nonTerminal)(i-1), conv1(nonTerminal))(x) }
    
  def precompute(len : Int) {
    for (nonTerminal <- grammar.toMap.keys) {
      for (i <- 2 to len) {
        getN(nonTerminal)(i)
      }

      for (i <- 0 to len;
           j <- 0 to len) {
        convCache(nonTerminal)(i)(j)
      }
    }
  }



  private def sumSubtractN(fOrig: Func, f1: Func, t: BigInt)(n : BigInt) = {
    fOrig(n) + f1(n - t)
  }
  
  private val convCache : Map[SingleType, SelfConvolutionCache] = grammar.toMap.mapValues { prods =>

    val summation = prods.foldLeft(sumIdentity) { 
      case (sumResult, rule) => 

        // Todo: this should use the convFunc and multiset?
        val conv = 
          if (rule.rightSide.isTerminal) d0 
          else rule.rightSide.nonTerminals.map(lazyConv1 _).reduceLeft(convolution(_, _))
        
        sumSubtractN(sumResult, conv, rule.rightSide.numTerminals)
    }
    
    val func = makeMemo { x => if (x < 0) 0 else summation(x) }
    MutableMap[Int, BigInt => BigInt](0 -> d0, 1 -> func)
  }.view.force


}