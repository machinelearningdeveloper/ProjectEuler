package com.machinelearningdeveloper.projecteuler

/** Problem 1 http://projecteuler.net/problem=1
  * "Find the sum of all the multiples of 3 or 5 below 1000."
  */

object Problem1 extends App {
  val multiplesOf3Or5 =
    for (i <- 1 until 1000 if (i % 3 == 0 || i % 5 == 0)) yield i
    
  println(multiplesOf3Or5 sum)
}