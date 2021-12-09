package com.github.cosminci.aoc._2021

import com.github.cosminci.aoc.utils

import scala.collection.mutable

object Day9 {
  def main(args: Array[String]): Unit = {
    val input = utils.loadInputAsStrings("2021/day9.txt").map(_.toCharArray.map(_ - '0').toSeq)

    println(s"Part I: ${riskLevel(input)}")
    println(s"Part II: ${largestBasinsProduct(input)}")
  }

  def riskLevel(grid: Seq[Seq[Int]]): Int = {
    val lowPoints = for {
      r <- grid.indices
      c <- grid(r).indices
      if utils.neighbours(grid, r, c).forall { case (r1, c1) => grid(r)(c) < grid(r1)(c1) }
    } yield (r, c)

    lowPoints.map { case (r, c) => grid(r)(c) + 1 }.sum
  }

  def largestBasinsProduct(grid: Seq[Seq[Int]]): Int = {
    val visited = mutable.Set.empty[(Int, Int)]
    def bfs(r: Int, c: Int): Int = {
      visited.add((r, c))
      val toVisit = mutable.Queue((r, c))
      var size    = 0
      while (toVisit.nonEmpty) {
        val (r, c) = toVisit.dequeue()
        size += 1
        utils.neighbours(grid, r, c).foreach { case (r1, c1) =>
          if (grid(r1)(c1) != 9 && !visited.contains((r1, c1))) {
            visited.add((r1, c1))
            toVisit.enqueue((r1, c1))
          }
        }
      }
      size
    }

    val sizes = for {
      r <- grid.indices
      c <- grid(r).indices
      if grid(r)(c) != 9 && !visited.contains((r, c))
    } yield bfs(r, c)

    sizes.sorted.takeRight(3).product
  }
}
