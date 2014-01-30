package com.machinelearningdeveloper.projecteuler

/** Problem 6 http://projecteuler.net/problem=6
  *  
  * "Find the difference between the sum of the squares of the
  * first one hundred natural numbers and the square of the sum."
  */

import language.postfixOps

object Project6 extends App {
  
  println(squareOfSumMinusSumOfSquares(100))
  
  def squareOfSumMinusSumOfSquares(n: Long) =
    sum(n, afterSumF = square) - sum(n, perElementF = square)
  
  def sum(n: Long, perElementF: (Long => Long) = {x => x}, afterSumF: Long => Long = {x => x}) =
    afterSumF((for (i <- 1L to n) yield perElementF(i)) sum)
    
  def square(x: Long) = x * x
    
}