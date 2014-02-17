package com.machinelearningdeveloper.projecteuler

/** Problem 18 http://projecteuler.net/problem=18
  * "Find the maximum total from top to bottom of the triangle below"
  *
  */

import scala.collection.mutable.PriorityQueue

object Problem18 extends App {
  case class Node(val idx: Int, val cost: Int)
  case class Path(val nodes: Vector[Node], val cost: Int) {
    override def toString: String =
      nodes.map(_.cost).mkString("->")
  }
  
  val digit = """(\d)""".r
  val triangle = """
    75
    95 64
    17 47 82
    18 35 87 10
    20 04 82 47 65
    19 01 23 75 03 34
    88 02 77 73 07 63 67
    99 65 04 28 06 16 70 92
    41 41 26 56 83 40 80 70 33
    41 48 72 33 47 32 37 16 94 29
    53 71 44 65 25 43 91 52 97 51 14
    70 11 33 28 77 73 17 78 39 68 17 57
    91 71 52 38 17 14 91 43 58 50 27 29 48
    63 66 04 68 89 53 67 30 73 16 69 87 40 31
    04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
    """.filter(_.toString match {
      case digit(d) => true
      case otherwise => false
    })
  
  val nodes = mkNodes(triangle)
  val levels = mkLevels(nodes)
  val maxLevel = levels.max
  val heuristicCost = nodes.map(_.cost).max
  val heuristics = nodes.toVector.indices.toVector.map {i => (maxLevel - levels(i)) * heuristicCost}
  val queue = new PriorityQueue[Path]()(Ordering.by {
    p: Path => p.cost
  })
  
  // Enqueue the root node
  queue.enqueue(new Path(Vector(nodes(0)), nodes(0).cost + heuristics(0)))

  // Use A* algorithm to build, enqueue, and dequeue paths until goal condition met
  val bestPath = walkPaths
  println(s"Best path: $bestPath (path cost: ${bestPath.cost})")

  def walkPaths: Path = {
    val path = queue.dequeue
    enqueueNextPaths(path)
    if (queue.isEmpty || levels(path.nodes.last.idx) == maxLevel) path
    else walkPaths
  }

  def enqueueNextPaths(path: Path) = {
    val frontier = path.nodes.last
    val level = levels(frontier.idx)
    if (level < maxLevel) {
      val leftPath = buildPath(path, nodes(frontier.idx + level))
      val rightPath = buildPath(path, nodes(frontier.idx + level + 1))
      queue.enqueue(leftPath)
      queue.enqueue(rightPath)
    }
  }
  
  def buildPath(path: Path, newNode: Node) = {
    new Path(path.nodes :+ newNode, path.nodes.map(_.cost).sum + newNode.cost + heuristics(newNode.idx))
  } 
  
  def mkNodes(digitString: String) = {
    (for {
      idxs <- digitString.indices.grouped(2)
      i = idxs(0)
      j = idxs(1)
    } yield new Node(i / 2, (digitString(i).asDigit * 10 + digitString(j).asDigit))).toVector
  }
    
  @scala.annotation.tailrec
  def mkLevels(nodes: Vector[Node], staircase: Vector[Int] = Vector.empty): Vector[Int] = {
    if (staircase.length >= nodes.length) staircase.slice(0, nodes.length)
    else if (staircase.length > 0) {
      val level = staircase.max + 1
      mkLevels(nodes, staircase ++ (1 to level).map(_ => level).toVector)
    } else {
      mkLevels(nodes, staircase :+ 1)
    }
  }
}