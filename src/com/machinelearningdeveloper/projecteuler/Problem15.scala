package com.machinelearningdeveloper.projecteuler

/** Problem 15 http://projecteuler.net/problem=15
  * "Starting in the top left corner of a 2x2 grid, and only being able to move to the right and down,
  * there are exactly 6 routes to the bottom right corner.
  * 
  * "How many such routes are there through a 20x20 grid?"
  */

object Problem15 extends App {
  
  val (width, length) = (20, 20)
  println(s"There are ${paths(width, length)} paths between opposite corners of a grid when movement is restricted to only two directions.")
  
  def paths(width: Int, length: Int) = 
    nChooseK(width + length, length)
  
  def nChooseK(n: Int, k: Int) = 
    factorial(n) / (factorial(n - k) * factorial(k))
    
  @scala.annotation.tailrec
  def factorial(n: BigInt, answer: BigInt = 1): BigInt = {
    assert(n >= 1, "n is not a positive integer")
    if (n == 1) answer
    else factorial(n - 1, answer * n)
  }

}