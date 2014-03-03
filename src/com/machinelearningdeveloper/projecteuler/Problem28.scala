package com.machinelearningdeveloper.projecteuler

/** Problem 28 http://projecteuler.net/problem=28
  * "What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral...?"
  */

object Problem28 extends App {
  val sideSize = 1001
  assert(sideSize % 2 > 0, s"sideSize ($sideSize) is even")
  
  println(diagSum(sideSize))
  
  def diagSum(sideSize: Int): Int = {
    val topRightCorner = sideSize * sideSize
    @scala.annotation.tailrec
    def calculateDiagSum(i: Int = 1, stepSize: Int = 2, sum: Int = 0): Int = {
      if (i == topRightCorner)
        i + sum
      else
        calculateDiagSum(i + (stepSize * 4), stepSize + 2, sum + (4 * i + 6 * stepSize))
    }
    calculateDiagSum()
  }
}