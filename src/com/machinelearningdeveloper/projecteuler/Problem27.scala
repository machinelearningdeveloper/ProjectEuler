package com.machinelearningdeveloper.projecteuler

object Problem27 extends App {
  val start = System.nanoTime
  val coeffs = testCoefficients(-1000, 1000).sortBy(_._2).last._1
  println("The sum of the coefficients a, b such that |a| < 1000 and |b| < 1000")
  println("resulting in the longest number of consecutive leading primes when")
  println(s"combined with n in n^2 + an + b is ${coeffs._1 * coeffs._2}")
  println(s"Took ${(System.nanoTime - start) / 1000000} ms")
  
  def testCoefficients(start: Int, end: Int) =
    for {
      a <- start + 1 until end
      b <- start + 1 until end
    } yield ((a, b), numLeadingPrimes(a, b))
  
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