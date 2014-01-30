package com.machinelearningdeveloper.projecteuler

/** Problem 4 http://projecteuler.net/problem=4
  *  
  * "Find the largest palindrome made from the product of two 3-digit numbers."
  */

import language.postfixOps

object Problem4 extends App {
  type Products = Vector[Int]

  println(palindromes(productsOfPairsInRange(100, 999)).max)
  
  def productsOfPairsInRange(start: Int, end: Int): Products = 
    (for {
      i <- start to end
      j <- start to end
      product = i * j
    } yield product).toVector distinct
    
  def palindromes(products: Products): Products =
    products filter(i => i.toString == i.toString.reverse)
}