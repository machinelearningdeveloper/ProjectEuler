package com.machinelearningdeveloper.eulerproject

/** Problem 2 http://projecteuler.net/problem=2
  *  
  * "By considering the terms in the Fibonacci sequence whose
  * values do not exceed four million, find the sum of the
  * even-valued terms."
  */

object Problem2 extends App {
  type Fibonacci = Int
  type Fibonaccis = Vector[Fibonacci]
  
  println(fibonaccis(4000000) filter(_ % 2 == 0) sum)
  
  /** The canonical formulation of the fibonacci sequence looks
    * something like the following:
    *  
    *    def fibonacci(n: Int): Int =
    *      if (n == 0 || n == 1)
    *        1
    *      else
    *        fibonacci(n - 2) + fibonacci(n - 1)
    *
    *  The sequence will be generated using tail recursion
    *  here, however, to better accommodate the calculation
    *  of larger values.
    */
  
  def fibonacci(n: Int): Fibonacci = {
    if (n < 0) sys.error(s"negative number ($n)")
    
    @scala.annotation.tailrec
    def accumulateSum(iters: Int = 1, acc: Fibonacci = 1, previous: Fibonacci = 1): Fibonacci =
      if (iters >= n)
        acc
      else
        accumulateSum(iters + 1, acc + previous, acc)
    accumulateSum()
  }
      
  /** All fibonacci numbers up to value */
  def fibonaccis(upToValue: Int): Fibonaccis = {
    /** Pack up the numbers using an immutable data structure */
    @scala.annotation.tailrec
    def accumulateFibonaccis(n: Int = 0, numbers: Fibonaccis = Vector.empty): Fibonaccis = {
      val nextFibonacci = fibonacci(n)
      if (nextFibonacci >= upToValue)
        numbers
      else
        accumulateFibonaccis(n + 1, numbers :+ nextFibonacci)
    }
    accumulateFibonaccis()
  }
}