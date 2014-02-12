package com.machinelearningdeveloper.projecteuler

/** Problem 12 http://projecteuler.net/problem=12
  * "What is the value of the first triangle number to have over five hundred divisors?"
  */

object Problem12 extends App {
  val goal = 500
  val timeBefore = System.nanoTime
  val triangle = triangles.filter(_ % 2 == 0).filter(factorsOfEvenNumber(_).size > goal).head
  println(triangle)
  val timeAfter = System.nanoTime
  println(s"Took ${(timeAfter - timeBefore) / 1000000} ms")
  
  def naturals: Stream[Long] = {
    def naturalAt(n: Long): Stream[Long] = 
      n #:: naturalAt(n + 1)
    naturalAt(1)
  }
  
  def triangles: Stream[Long] = naturals.scanLeft(0L)((acc, value) => acc + value).drop(1)

  def factorsOfEvenNumber(number: Long) = {
    assert(number % 2 == 0, s"number ($number) is not even")
    def checkFactors(dividend: Long, divisor: Long = 3,
                     factors: Set[Long] = Set(1, 2, number)): Set[Long] = {
      val quotient = dividend / divisor
      if (divisor > Math.sqrt(dividend))
        factors
      else
        checkFactors(dividend, divisor + 1, factors ++
                     (if (dividend % divisor == 0) Set(divisor, quotient) else Set.empty))
    }
    checkFactors(number)
  }
}