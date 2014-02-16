package com.machinelearningdeveloper.projecteuler

/** Problem 16 http://projecteuler.net/problem=16
  * "What is the sum of the digits of the number 2 [to the 1000th power]?"
  */

object Problem16 extends App {
  println(powersOfTwo(1000).toString.map(_.asDigit).sum )
  
  @scala.annotation.tailrec
  def powersOfTwo(exponent: Int = 0, answer: BigInt = 2): BigInt = {
    assert(exponent >= 0, "exponent is less than zero")
    if (exponent == 0) 1
    else if (exponent == 1) answer
    else powersOfTwo(exponent - 1, answer * 2)
  }
    
}