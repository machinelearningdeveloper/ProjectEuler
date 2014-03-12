package com.machinelearningdeveloper.projecteuler

/** Problem 31 http://projecteuler.net/problem=31
  * "How many different ways can [2 pounds (200 pence)] be made using any number of coins [in 1, 2, 5, 10, 20, 50, 100, 200]?"
  */

object Problem31 extends App {
  println(s"There are ${countCoinCombinations(200, Vector(1, 2, 5, 10, 20, 50, 100, 200))} different combinations of coins that add up to 2 pounds.")
  
  def countCoinCombinations(targetSum: Int, denominations: Vector[Int], sum: Int = 0): Int = {
    if (targetSum == 0)
      sum + 1
    else if (targetSum < 0 || denominations.isEmpty)
      sum
    else
      countCoinCombinations(targetSum - denominations.head, denominations, sum) +
      countCoinCombinations(targetSum, denominations.tail, sum)
  }
}