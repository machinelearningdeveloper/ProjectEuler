package com.machinelearningdeveloper.projecteuler

/** Problem 30 http://projecteuler.net/problem=30
  * "Find the sum of all the numbers that can be written as the sum of fifth powers of their digits."
  */

object Problem30 extends App {
  val places = 5
  val loBound = math.pow(1, places).toLong + math.pow(2, places).toLong
  val hiBound = calculateHiBound(places)
  
  println(nsEqualToSumsOfPowersOfDigits(loBound, hiBound, places).sum)
  
  def nsEqualToSumsOfPowersOfDigits(loBound: Long, hiBound: Long, places: Int) =
    for {
      n <- loBound to hiBound
      if (sumDigits(n) == n)
    } yield n
  
    @scala.annotation.tailrec
    def calculateHiBound(places: Int): Long = {
      val nines = "9" * places
      val sum = sumDigits(nines.toLong)
      if (sum.toString.length >= nines.length)
        sum
      else
        calculateHiBound(places + 1)
    }
    
    def sumDigits(n: Long): Long = n.toString.map(d => math.pow(d.asDigit, places).toLong).sum
}