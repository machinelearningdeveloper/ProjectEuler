package com.machinelearningdeveloper.projecteuler

/** Problem 12 http://projecteuler.net/problem=12
  * "What is the value of the first triangle number to have over five hundred divisors?"
  */

object Problem12 extends App {
  val goal = 500
  val idx = triangles.takeWhile(t => factors(t).size < goal).length
  println(idx)
  println(triangles(idx))
  
  def naturals: Stream[Long] = {
    def naturalAt(n: Long): Stream[Long] = 
      n #:: naturalAt(n + 1)
    naturalAt(1)
  }
  
  def triangles: Stream[Long] = naturals.scanLeft(0L)((acc, value) => acc + value).drop(1)
  
  def factors(n: Long) = {
    def checkFactors(dividend: Long, divisor: Long = 2, factors: Set[Long] = Set(1)): Set[Long] = {
      val quotient = dividend / divisor
      if (divisor > Math.sqrt(dividend))
        factors + dividend
      else
        checkFactors(dividend, divisor + 1, factors ++ (if (dividend % divisor == 0) Set(divisor, quotient) else Set.empty))
    }
    checkFactors(n)
  }
}