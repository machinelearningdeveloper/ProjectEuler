package com.machinelearningdeveloper.projecteuler

/** Problem 20 http://projecteuler.net/problem=20
  *  "Find the sum of the digits in the number 100!"
  */

object Problem20 extends App {  
  println(factorial(100).toString.map(_.asDigit).sum)
  
  def factorial(n: Int, result: BigInt = 1): BigInt =
    if (n == 1) result else factorial(n - 1, result * n)
}