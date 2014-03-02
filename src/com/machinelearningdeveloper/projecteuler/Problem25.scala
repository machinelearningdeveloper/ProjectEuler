package com.machinelearningdeveloper.projecteuler

/** Problem 25 http://projecteuler.net/problem=25
  * "What is the first term in the Fibonacci sequence to contain 1000 digits?"
  */

object Problem25 extends App {
  println(getFibArgResultingInFibOfLength(1000))
  
  def getFibArgResultingInFibOfLength(targetLen: Int): Int = {
    @scala.annotation.tailrec
    def checkFibLength(i: Int = 1): Int = {
      if (fibonacci(i).toString.length >= targetLen)
        i
      else
        checkFibLength(i + 1) 
    }
    checkFibLength()
  }
  
  def fibonacci(n: Int): BigInt = {
    @scala.annotation.tailrec
    def memoizeFibs(i: Int = 3, memoizedFibs: (BigInt, BigInt) = (1, 1)): BigInt = {
      val (lookBack2, lookBack1) = memoizedFibs
      val fib = lookBack2 + lookBack1
      if (n < i)
        1
      else if (i == n)
        fib
      else
        memoizeFibs(i + 1, (lookBack1, fib))
    }
    memoizeFibs()
  }
}