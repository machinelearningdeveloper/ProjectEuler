package com.machinelearningdeveloper.projecteuler

/** Problem 4 http://projecteuler.net/problem=5
  *  
  * "What is the smallest positive number that is
  * evenly divisible by all of the numbers from 1 to 20?"
  */

object Problem5 extends App {
  println(findLeastCommonMultiple(20))

  def findLeastCommonMultiple(upToNumber: Long, currentStep: Int = 0): Long = {
    if (upToNumber <= 0) sys.error(s"upToNumber ($upToNumber) must be positive")
    @scala.annotation.tailrec
    def stepThroughMultiples(multiple: Long, divisor: Long = 2): Long = {
      if (divisor == upToNumber) multiple
      else if (multiple % divisor == 0) stepThroughMultiples(multiple, divisor + 1)
      else stepThroughMultiples(multiple + upToNumber)
    }
    stepThroughMultiples(upToNumber + upToNumber)
  }
}