package com.machinelearningdeveloper.projecteuler

/** Problem 22 http://projecteuler.net/problem=22
  * "What is the total of all the name scores in the file [resources/names.txt]?"
  */

object Problem22 extends App {
  val collectionChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ,"
  val namesSource = scala.io.Source.
    fromFile("resources/names.txt")
    
  val names = namesSource.
    getLines.toVector.mkString(", ").filter(collectionChars.contains(_)).
    split(",").sorted.zipWithIndex
    
  namesSource.close
  
  val nameScores = names.map {
    t =>
      val (name, index) = t
      name.map(c => c - 'A' + 1).sum * (index + 1)
  }
  
  println(nameScores.reduce(_+_))
}