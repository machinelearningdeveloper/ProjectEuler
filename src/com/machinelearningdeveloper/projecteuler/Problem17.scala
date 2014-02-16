package com.machinelearningdeveloper.projecteuler

/** Problem 17 http://projecteuler.net/problem=17
  * "If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?"
  */

object Problem17 extends App {
  val divisors = Vector(1e9, 1e6, 1e3, 1e0).map { _.toInt }
  val magnitudes = Vector("billion", "million", "thousand")
  val numbers = Vector("one", "two", "three", "four", "five", "six", "seven",
                       "eight", "nine", "ten", "eleven", "twelve", "thirteen",
                       "fourteen", "fifteen", "sixteen", "seventeen", "eighteen",
                       "nineteen", "twenty", "thirty", "forty", "fifty", "sixty",
                       "seventy", "eighty", "ninety")
  val letter = """(\w)""".r

  println((1 to 1000).map(convert(_)).map(_.filter(_.toString match {
      case letter(l) => true
      case otherwise => false
    }).length).sum)
  
  def convert(n: Int) =
    (if ((n & (1 << 31)) != 0) "negative " else "") +
    (numberComponents(n).map(hundredsAndRemainder(_)).zipWithIndex.filter(_._1.length > 0).map {
      t => if (t._1.length > 0 && t._2 < magnitudes.length) Vector(t._1, magnitudes(t._2)).mkString(" ") else t._1 
    }).mkString(" ")
                       
  @scala.annotation.tailrec
  def numberComponents(n: Int, components: Vector[Int] = Vector.empty): Vector[Int] = {
    if (components.length == divisors.length) components
    else {
      val divisor = divisors(components.length)
      val quotient = n / divisor
      numberComponents(n % divisor, components :+ (if (quotient != 0) math.abs(quotient) else 0))
    }
  }
  
  def hundredsAndRemainder(n: Int) = {
    (hundreds(n) ++ remainder(n)).mkString(" and ") 
  }
  
  def hundreds(n: Int): Vector[String] = {
    val hundreds = n / 100
    val hundredsString = if (hundreds > 0) s"${numbers(hundreds - 1)} hundred"
    else ""
    if (hundredsString.length > 0) Vector(hundredsString) else Vector.empty
  }
  
  def remainder(n: Int): Vector[String] = {
    val tens = n % 100
    val ones = n % 10
    val remainderString = if (tens >= 20) s"${numbers(19 + (tens / 10 - 2))}" +
                   (if (ones > 0) s"-${numbers(ones - 1)}" else "")
    else if (tens >= 10) s"${numbers(tens - 1)}" 
    else if (ones > 0) s"${numbers(ones - 1)}"
    else ""
    if (remainderString.length > 0) Vector(remainderString) else Vector.empty
  }
}