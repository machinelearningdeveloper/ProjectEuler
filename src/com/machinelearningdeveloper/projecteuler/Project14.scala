package com.machinelearningdeveloper.projecteuler

/** Problem 14 http://projecteuler.net/problem=14
  * "Which starting number, under one million, produces the longest chain?"
  * 
  *  Chains start at a value and end at 1, subject to step rules:
  *      n / 2 (even)
  *      3n + 1 (odd)
  */

object Project14 extends App {
  val start = 1
  val upperBound = 1000000
  
  val timeBefore = System.nanoTime
  val (argmax, maximum) = maxStepsInRange(from = start, until = upperBound)
  println(s"The longest chain of steps using the Collatz rules for n in range $start until $upperBound: $maximum")
  println(s"The starting value of the longest chain: $argmax")
  val timeAfter = System.nanoTime
  println(s"Took ${(timeAfter - timeBefore) / 1000000} ms")
  
  def stepF = Vector(((n: Long) => n / 2), ((n: Long) => 3 * n + 1))
  
  def valueAtStep(n: Long) = stepF((n % 2).toInt)(n)
  
  @scala.annotation.tailrec
  def stepsToOne(n: Long, numberOfSteps: Int = 1): Int = {
    assert(n >= 1, s"$n")
    if (n == 1) numberOfSteps
    else stepsToOne(valueAtStep(n), numberOfSteps + 1)
  }
    
  @scala.annotation.tailrec
  def maxStepsInRange(from: Int = 1, until: Int = 1, argMax: Int = 1, max: Int = 1): (Int, Int) =  {
    assert(from <= until, s"from ($from) greater than until ($until)")
    if (from == until) (argMax, max)
    else {
      val numberOfSteps = stepsToOne(from)
      val (newArgMax, newMax) = if (numberOfSteps > max) (from, numberOfSteps) else (argMax, max) 
      maxStepsInRange(from + 1, until, newArgMax, newMax)
    }
  }
}