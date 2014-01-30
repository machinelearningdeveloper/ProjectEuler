package com.machinelearningdeveloper.projecteuler

/** Problem 9 http://projecteuler.net/problem=9
  * "There exists exactly one Pythagorean triplet for which a + b + c = 1000.
  * Find the product abc."
  */

object Problem9 extends App {
  val goalValue = 1000
  val triplet = tripletsEqualToSum(pythagoreanTriplets(goalValue), goalValue).head

  println(triplet._1 * triplet._2 * triplet._3)
  
  def tripletsEqualToSum(triplets: Vector[(Int, Int, Int)], sum: Int) = 
    triplets filter (t => t._1 + t._2 + t._3 == sum)
  
  def pythagoreanTriplets(maxValue: Int) = {
    for {
      a <- 1 to maxValue
      b <- a + 1 to maxValue
      c <- b + 1 to maxValue
      p = (a * a + b * b == c * c)
      if (a < b && b < c && p)
    } yield (a, b, c)
  } toVector
}