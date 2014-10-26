package com.jakemadethis.grammargen;

import scala.collection.mutable.{Map => MutableMap}

trait Convolutor {
  def precompute(len : Int): Unit
  def getN(n : Int): BigInt => BigInt
}

class MemoizableConvolutor(func : BigInt => BigInt) extends Convolutor {

  import util._

  private val convFuncs = MutableMap[Int, BigInt => BigInt](0 -> d0, 1 -> func)

  private def selfConvolution(i : Int) = 
    makeMemo { x => convolution(convFuncs(i-1), func)(x) }
    
  def precompute(len : Int) {
    for (i <- 2 to len) {
      getN(i)
    }

    for (i <- 0 to len;
         j <- 0 to len) {
      convFuncs(i)(j)
    }
  }

  def getN(n : Int) = convFuncs.getOrElseUpdate(n, selfConvolution(n))
}