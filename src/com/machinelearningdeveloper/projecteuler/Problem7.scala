package com.machinelearningdeveloper.projecteuler

/** Problem 7 http://projecteuler.net/problem=7
  *  
  * "What is the 10 001st prime number?"
  */

/** The solution to problem 3 can be used to test primality:
 *  largestFactor tries to find the largest factor of n
 *  such that the factor is less than n, otherwise
 *  it returns n (in the case n is prime)
 */
import Problem3.largestFactor

object Problem7 extends App {
  type Primes = Vector[Long]
  
  println(primes(10001) last)
  
  @scala.annotation.tailrec
  def primes(numberOfPrimes: Int, candidate: Long = 2, ps: Primes = Vector.empty): Primes = {
    if (ps.length == numberOfPrimes) {
      ps
    } else {
      val nextCandidate = candidate + (if (candidate > 2) 2 else 1)
      val prime =
        if (largestFactor(candidate) == candidate) Vector(candidate)
        else Vector.empty

      primes(numberOfPrimes, nextCandidate, ps ++ prime)
    }
  }

}