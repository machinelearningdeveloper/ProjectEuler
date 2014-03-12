package com.machinelearningdeveloper.projecteuler

/** Problem 32 http://projecteuler.net/problem=32
  * "Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital."
  */

object Problem32 extends App {
  
  println(s"The sum of all 1 through 9 pandigital numbers is ${sumPandigitals}")
  
  def sumPandigitals: Int = {
    (for {
      multiplicand <- 1 to 99
      multiplier <- 100 to 9999
    } yield (multiplicand, multiplier)).filter(t => isPandigital(t._1, t._2)).map(t => t._1 * t._2).distinct.sum
  }
  
  def isPandigital(multiplicand: Int, multiplier: Int): Boolean = {
    val product = multiplicand * multiplier
    if (multiplicand * multiplier != product)
      false
    else {
      val digits = (multiplicand.toString + multiplier.toString + product.toString)
      digits.length == 9 && digits.filter(_ != '0').length == 9 && digits.toSet.size == 9
    }
  }
}