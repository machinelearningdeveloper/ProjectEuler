package com.machinelearningdeveloper.projecteuler

/** Problem 23 http://projecteuler.net/problem=23
  * "Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers."
  */

object Problem23 extends App {
  val limit = 28123  // All integers > 28123 are sums of two abundant numbers
  val testRange = (1 to limit)
  val abundantNumbers = testRange.filter(isAbundant(_)).toVector
  val sums = distinctSumsOfPairs(abundantNumbers)
  
  println(s"Sum of positive integers that are not sums of abundant numbers: ${testRange.filter(!sums.contains(_)).sum}")
  
  def divisors(n: Int) = {
    val sqrtN = math.sqrt(n)
    val step = if (n % 2 == 0) 1 else 2
    def buildDivisors(divisor: Int = 1 + step, divisors: Set[Int] = Set(1)): Set[Int] = {
      val testNextDivisor = buildDivisors(divisor + step, _: Set[Int])
      if (divisor > sqrtN)
        divisors
      else
        testNextDivisor((if (n % divisor == 0) (divisors + divisor) + (n / divisor) else divisors))
    }
    buildDivisors()
  }

  def isAbundant(n: Int) = n < divisors(n).sum
  
  def distinctSumsOfPairs(numbers: Vector[Int]) =
    (for {
      m <- numbers
      n <- numbers
      if (m <= n && m + n <= limit)
    } yield m + n).distinct
}