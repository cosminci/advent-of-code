package com.github.cosminci.aoc._2021

import com.github.cosminci.aoc.utils

import scala.annotation.tailrec
import scala.collection.mutable

object Day11 {
  def main(args: Array[String]): Unit = {
    val grid = utils.loadInputAsStrings("2021/day11.txt").map(_.toCharArray.map(_ - '0')).toArray

    println(s"Part I: ${countFlashes(grid.map(_.clone()), days = 100)}")
    println(s"Part II: ${judgementDay(grid)}")
  }

  def countFlashes(grid: Array[Array[Int]], days: Int): Int =
    (1 to days).foldLeft(0)((count, _) => count + update(grid))

  def judgementDay(grid: Array[Array[Int]]): Int = {
    @tailrec
    def dfs(day: Int): Int =
      if (update(grid) == grid.length * grid.head.length) day else dfs(day + 1)

    dfs(day = 1)
  }

  private def update(grid: Array[Array[Int]]): Int = {
    val coords = for {
      x <- grid.indices
      y <- grid(x).indices
    } yield (x, y)

    coords.foreach { case (x, y) => grid(x)(y) += 1 }
    val visited = mutable.Set.empty[(Int, Int)]
    val toVisit = mutable.PriorityQueue.from(coords.filter { case (x, y) => grid(x)(y) == 10 })

    while (toVisit.nonEmpty) {
      val (x, y) = toVisit.dequeue()
      visited.add((x, y))
      grid(x)(y) = 0
      utils.neighbours(grid.length - 1, grid.head.length - 1, x, y, includeDiagonals = true).foreach { case (x1, y1) =>
        if (!visited.contains((x1, y1))) {
          grid(x1)(y1) += 1
          if (grid(x1)(y1) == 10) {
            visited.add((x1, y1))
            toVisit.enqueue((x1, y1))
          }
        }
      }
    }

    visited.size
  }

}
