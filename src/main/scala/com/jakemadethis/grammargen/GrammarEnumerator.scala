package com.jakemadethis.grammargen;
import scala.collection.mutable.{Map => MutableMap}
import scala.collection.mutable.Buffer
import scala.collection.mutable.ArrayBuffer

class GrammarEnumerator[D](val grammar: Grammar[D]) {
  
  import util._

  val conv = MutableMap[D, Convolutor]()
  private def conv1(nt: D) = conv(nt).getN(1)
  private def lazyConv1(nt : D)(x : BigInt) = conv1(nt)(x)

  def count(nt: D, len : Int) = conv1(nt)(len)
  
  def countAll(set: MultiSet[D], len : Int) : BigInt = {
    if (set.isEmpty) return d0(len)
    set.map { case (nt, num) => conv(nt).getN(num) }
      .reduce { convolution(_,_) }(len)
  }
  
  def count(prod : Form[D], len : Int) = countAll(prod.nonTerminalSet, len - prod.numTerminals)
  
  def countRange(derivation : Form[D], min : Int, max : Int) = 
    (min to max).foldLeft(BigInt(0)) { case (result, i) => result + count(derivation, i) }
  
  def precompute(len : Int) {
    for (i <- 0 to len) {
      grammar.map.keys.foreach(conv1(_)(i))
    }

    for (nt <- grammar.map.keys) {
      conv(nt).precompute(len)
    }
  }
  
  //

  
  private def sumSubtractN(fOrig: Func, f1: Func, t: BigInt)(n : BigInt) = {
    fOrig(n) + f1(n - t)
  }
  
  grammar.map.foreach { case (nt, prods) =>

    val summation = prods.foldLeft(sumIdentity) { 
      case (sumResult, rule) => 

        // Todo: this should use the convFunc and multiset?
        val conv = 
          if (rule.form.isTerminal) d0 
          else rule.form.nonTerminals.map(lazyConv1 _).reduceLeft(convolution(_, _))
        
        sumSubtractN(sumResult, conv, rule.form.numTerminals)
    }
    
    val func = makeMemo { x => if (x < 0) 0 else summation(x) }
    conv(nt) = new MemoizableConvolutor(func)
  }


}

