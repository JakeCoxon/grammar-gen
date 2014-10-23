package com.jakemadethis.grammargen;
import scala.collection.mutable.{Map => MutableMap}
import scala.util.Random
import scala.collection.mutable.Buffer
import scala.collection.mutable.ArrayBuffer

private object util {
  
  def makeDelta(n: BigInt) : BigInt => BigInt = {
    x: BigInt => if (x == n) 1 else 0
  }
  val d0 = makeDelta(0)
  val sumIdentity : BigInt => BigInt = a => 0
  
  def random(max : BigInt, rng : Random) : BigInt = {
    val r = BigInt(max.bitLength, rng)
    if (r <= max) r else random(max, rng)
  }
  
  type Func = BigInt => BigInt
  
  /** convolution(f1, f2) makes a function f(t) that performs a convolution 
   *  from 0 to t.
   *  It is assumed that f1(0) = 0 and f2(0) = 0  else infinite loop
   */
  def convolution(f1: Func, f2: Func)(num : BigInt) : BigInt = {
    (BigInt(1) to num - 1).map { i => f1(i) * f2(num-i) }.sum
  }
  
  /** Makes a convolution of `f' with itself `reps' times.
   *  If reps is 0 then returns d0 the identity function **/
  def selfConvolution(f : Func, reps : Int) : Func =
    if (reps == 0) d0
    else (1 until reps).foldLeft(f) { (result, _) => convolution(result, f) }
  
  def makeMemo(f: Func): Func = {
    val map = MutableMap[BigInt, BigInt]()
    x: BigInt => map.getOrElseUpdate(x, f(x))
  }
}

class GrammarEnumerator[D](val grammar: Grammar[D]) {
  
  import util._

  val funcs = MutableMap[D, Func]()
  val convFuncs = MutableMap[D, collection.Map[Int, Func]]()
  
  
  def count(nt: D, len : Int) = funcs(nt)(len)
  
  def countAll(set: MultiSet[D], len : Int) : BigInt = {
    if (set.isEmpty) return d0(len)
    set.map { case (k, num) => convFuncs(k)(num) }
      .reduce { convolution(_,_) }(len)
  }
  
  def count(prod : Form[D], len : Int) = countAll(prod.nonTerminalSet, len - prod.numTerminals)
  
  def countRange(derivation : Form[D], min : Int, max : Int) = 
    (min to max).foldLeft(BigInt(0)) { case (result, i) => result + count(derivation, i) }
  
  // private def convolutionStream(n : D, i : Int) : Stream[Func] = 
  //       makeMemo { x => convolution(convFuncs(n)(i-1), funcs(n))(x) } #:: convolutionStream(n, i+1)

  private def convolutionStream(func: Func, n : D) : Stream[Func] = {

    def loop(i : Int) : Stream[Func] = 
      makeMemo { x => convolution(convFuncs(n)(i-1), func)(x) } #:: loop(i)
    
    d0 #:: func #:: loop(2)
  }

  def precompute(len : Int) {
    for (i <- 0 to len) funcs.values.foreach(_.apply(i))
    
    for (n <- funcs.keys) {

      
      val seq = convolutionStream(funcs(n), n)

      val convFuncMap = (0 to len zip seq).toMap

      // val convFuncMap = (0 to len).map { 
      //   case 0 => (0 -> d0)
      //   case 1 => (1 -> funcs(n))
      //   case i => 
      //     val func = makeMemo { x => convolution(convFuncs(n)(i-1), funcs(n))(x) }
      //     (i -> func)
      // }.toMap

      convFuncs(n) = convFuncMap

      for (i <- 0 to len;
           j <- 0 to len) {
        convFuncs(n)(i)(j)
      }
    }
  }
  
  //

  
  private def getf(nt : D)(x : BigInt) = funcs(nt)(x)
  private def sumSubtractN(fOrig: Func, f1: Func, t: BigInt)(n : BigInt) = {
    fOrig(n) + f1(n - t)
  }
  
  grammar.map.foreach { case (nt, prods) =>
    val summation = prods.zipWithIndex.foldLeft(sumIdentity) { 
      case (sumResult, (rule, prodId)) => 

        // Todo: this should use the convFunc and multiset?
        val conv = 
          if (rule.form.isTerminal) d0 
          else rule.form.nonTerminals.map(getf _).reduceLeft(convolution(_, _))
        
        sumSubtractN(sumResult, conv, rule.form.numTerminals)
    }
    
    funcs(nt) = makeMemo { x => if (x < 0) 0 else summation(x) }
  }

}

// abstract class Generator[D <: Form] {
//   def derive(nonTerminal : Grammar.Symbol, derivation : D)
// }

