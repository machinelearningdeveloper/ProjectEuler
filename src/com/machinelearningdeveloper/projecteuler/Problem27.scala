package com.machinelearningdeveloper.projecteuler

object Problem27 extends App {
  val startTime = System.nanoTime
  val limit = 1000
  val sharedTxt = "leading primes when\ncombined with n in n^2 + an + b"
  val answer = testCoefficients(-limit, limit)
  if (answer.isEmpty)
    println(s"No coefficients in (-limit, limit) produce $sharedTxt.")
  else {
    val coeffs = answer.get
    println(s"The product of the coefficients a, b such that |a| < $limit and |b| < $limit")
    println(s"resulting in the longest number of consecutive $sharedTxt is ${coeffs._1 * coeffs._2}.")
  }
  println(s"Took ${(System.nanoTime - startTime) / 1000000} ms")
  
  def testCoefficients(start: Int, end: Int): Option[(Int, Int)] = {
    @scala.annotation.tailrec
    def findLongestRunOfLeadingPrimes(a: Int, b: Int, bestCoeffs: Option[(Int, Int)] = None, longestRun: Int = 0): Option[(Int, Int)] = 
      if (a == end && b == end)
        bestCoeffs
      else {
        val runSize = numLeadingPrimes(a, b)
        val (bestAB, longestR) = if (runSize > longestRun) (Some((a, b)), runSize) else (bestCoeffs, longestRun)
        val (nextA, nextB) = if (b == end) (a + 1, start + 1) else (a, b + 1)
        findLongestRunOfLeadingPrimes(nextA, nextB, bestAB, longestR)
      }
    findLongestRunOfLeadingPrimes(start + 1, start + 1)
  }
  
  def numLeadingPrimes(a: Int, b: Int): Int = {
    @scala.annotation.tailrec
    def iterateN(n: Int = 0, numLeadingPrimes: Int = 0): Int = {
      if (!isPrime(quadForm(n, a, b)))
        numLeadingPrimes
      else
        iterateN(n + 1, numLeadingPrimes + 1)
    }
    iterateN()
  }
    
  def quadForm(n: Int, a: Int, b: Int) =
    n * n + a * n + b
  
  def isPrime(n: Int): Boolean = {
    if (n < 2 || (n > 2 && n % 2 == 0) || (n > 3 && n % 3 == 0))
      false
    else
      checkForPrimality(n)
  }
  
  def checkForPrimality(n: Int): Boolean = {
    val sqrtN = math.sqrt(n)
    @scala.annotation.tailrec
    def checkDivisors(divisor: Int = 3): Boolean =
      if (divisor > sqrtN)
        true
      else if (n % divisor == 0)
        false
      else
        checkDivisors(divisor + 2)
        
    if (n == 2 || n == 3)
      true
    else if (n % 2 == 0)
      false
    else
    checkDivisors()
  }
}