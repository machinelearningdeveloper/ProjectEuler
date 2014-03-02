package com.machinelearningdeveloper.projecteuler

/** Problem 21 http://projecteuler.net/problem=21
  * "Evaluate the sum of all the amicable numbers under 10000."
  *
  */ 

object Problem21 extends App {
  println((1 to 10000).flatMap(amicables(_)).distinct.sum)

  def divisors(n: Int) = {
    val sqrt = math.sqrt(n)
    val step = if (n % 2 == 0) 1 else 2
    @scala.annotation.tailrec
    def buildDivisors(divisor: Int = 2, divisors: Set[Int] = Set(1)): Set[Int] =
      if (divisor > sqrt)
        divisors
      else if (n % divisor == 0)
        buildDivisors(divisor + step, (divisors + divisor) + (n / divisor))
      else
        buildDivisors(divisor + step, divisors)
    buildDivisors()
  }
  
  def amicables(x: Int): Set[Int] = {
    val y = divisors(x).sum
    if (divisors(y).sum == x && x != y)
      Set(x, y)
    else
      Set.empty
  }
}