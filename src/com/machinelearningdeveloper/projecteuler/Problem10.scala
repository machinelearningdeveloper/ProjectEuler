package com.machinelearningdeveloper.projecteuler

/** Problem 10 http://projecteuler.net/problem=10
  * "Find the sum of all the primes below two million."
  */

/** The solution to problem 3 can be used to test primality:
  *  largestFactor tries to find the largest factor of n
  *  such that the factor is less than n, otherwise
  *  it returns n (in the case when n is prime)
  */
import Problem3.largestFactor

object Problem10 extends App {
  println(sumOfPrimes(2000000))
  
  @scala.annotation.tailrec
  def sumOfPrimes(upTo: Long, candidate: Long = 2, sum: Long = 0): Long = {
    if (candidate > upTo) {
      sum
    } else {
      val nextCandidate = candidate + (if (candidate == 2) 1 else 2)
      val prime =
        if (candidate == largestFactor(candidate))
          candidate
        else
          0
      sumOfPrimes(upTo, nextCandidate, sum + prime)
    }
    
  }
}