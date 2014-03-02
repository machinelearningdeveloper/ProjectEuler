package com.machinelearningdeveloper.projecteuler

/** Problem 24 http://projecteuler.net/problem=24
  * "What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?"
  */
object Problem24 extends App {
  // Note: the call to toVector is expensive and the use of sorted is unnecessary.
  // A faster solution drops the first 999999 elements of the iterator returned
  // by permutations and calls next on it.
  val permutations = "0123456789".permutations.toVector.sorted
  println(permutations(999999))
}