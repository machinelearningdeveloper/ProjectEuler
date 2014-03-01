package com.machinelearningdeveloper.projecteuler

/** Problem 20 http://projecteuler.net/problem=20
  *  "Find the sum of the digits in the number 100!"
  */

object Problem20 extends App {
  println((1 to 100).map(BigInt(_)).product.toString.map(_.asDigit).sum)
}