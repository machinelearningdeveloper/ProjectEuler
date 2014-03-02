package com.machinelearningdeveloper.projecteuler

/** Problem 26 http://projecteuler.net/problem=26
  * "Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part."
  */

object Problem26 extends App {
  println((1 until 1000).map(d => (d, cycleLength(d))).sortBy(_._2).last._1)
  
  @scala.annotation.tailrec
  def cycleLength(d: Int, r: Int = 1, run: Map[(Int, Int), Int] = Map.empty): Int = {
    if (r == 0)  
      0
    else {
      @scala.annotation.tailrec
      def expandR(r: Int): Int = if (r > d) r else expandR(r * 10)
      val s = expandR(r)
      val value = s / d
      val rem = s % d
      val index = run.getOrElse((value, rem), -1)
      if (index >= 0) {
        run.size - index
      } else cycleLength(d, rem, run + ((value, rem) -> run.size))
    }
  }
}
