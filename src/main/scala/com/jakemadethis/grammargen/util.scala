package com.jakemadethis.grammargen;
import scala.collection.mutable.{Map => MutableMap}
import scala.util.Random

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