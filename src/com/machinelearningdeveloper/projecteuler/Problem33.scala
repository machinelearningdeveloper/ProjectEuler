package com.machinelearningdeveloper.projecteuler

/** Problem 33 http://projecteuler.net=problem33
  * "There are exactly four non-trivial examples of this type of fraction, less than one in value,
  *  and containing two digits in the numerator and denominator."
  */

object Problem33 extends App {
  val fractions = cancellableFractions
  val productNumerator = fractions.map(_._1).product
  val productDenominator = fractions.map(_._2).product
  val greatestCommonDivisor = gcd(productNumerator, productDenominator)
  println(s"${productDenominator / greatestCommonDivisor}")
  
  @scala.annotation.tailrec
  def gcd(numerator: Int, denominator: Int): Int = {
    val remainder = denominator % numerator
    if (remainder == 0)
      numerator
    else
      gcd(denominator, remainder)
  }
  
  def cancellableFractions = 
    for {
      i <- 10 to 99
      j <- 10 to 99
      if (i < j && cancelledEqualsOriginalFraction(i, j))
    } yield (i, j)
    
  def cancelledEqualsOriginalFraction(i: Int, j: Int) = {
    val iTens = (i / 10).toDouble
    val iOnes = (i - iTens * 10).toDouble
    val jTens = (j / 10).toDouble
    val jOnes = (j - jTens * 10).toDouble
    val iDouble = i.toDouble
    val jDouble = j.toDouble
    (iTens / jOnes == iDouble / jDouble) && iOnes == jTens
  }
}