package com.machinelearningdeveloper.eulerproject

/** Problem 3 http://projecteuler.net/problem=3
  * "What is the largest prime factor of the number 600851475143?"
  */

object Problem3 extends App {
  val numberToFactor = 600851475143L
  
  println(largestPrimeFactor(numberToFactor))
  
  @scala.annotation.tailrec
  def largestPrimeFactor(n: Long): Long = {
    val factor = largestFactor(n)
    if (factor == n) n
    else largestPrimeFactor(factor)
  }
  
  @scala.annotation.tailrec
  def largestFactor(dividend: Long, divisor: Long = 2): Long =
    if (divisor > math.sqrt(dividend))
      dividend
    else if (dividend % divisor == 0)
      dividend / divisor
    else
      largestFactor(dividend, divisor + (if (divisor > 2) 2 else 1))
}