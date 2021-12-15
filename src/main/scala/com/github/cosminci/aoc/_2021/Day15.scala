package com.github.cosminci.aoc._2021

import com.github.cosminci.aoc.utils

import scala.collection.mutable
import scala.math.Integral.Implicits._

object Day15 {
  def main(args: Array[String]): Unit = {
    val grid = utils
      .loadInputAsStrings("2021/day15.txt")
      .map(_.toCharArray.map(_ - '0').toSeq)

    println(s"Part I: ${lowestTotalRisk(grid)}")
    println(s"Part II: ${lowestTotalRisk(generateCompleteMap(grid))}")
  }

  def lowestTotalRisk(grid: Seq[Seq[Int]]): Int = {
    val (n, m)  = (grid.length - 1, grid.head.length - 1)
    val toVisit = mutable.PriorityQueue((0, 0, 0))
    val visited = mutable.Set((0, 0))

    while (toVisit.nonEmpty) {
      val (risk, x0, y0) = toVisit.dequeue()
      if (x0 == n && y0 == m) return -risk

      utils.neighbours(n, m, x0, y0).collect {
        case (x1, y1) if !visited.contains((x1, y1)) =>
          toVisit.enqueue((risk - grid(x1)(y1), x1, y1))
          visited.add((x1, y1))
      }
    }

    Int.MaxValue
  }

  def generateCompleteMap(grid: Seq[Seq[Int]]): Seq[Seq[Int]] = {
    val (n, m) = (grid.length, grid.head.length)
    Seq.tabulate(5 * n, 5 * m) { case (i, j) =>
      val (xTile, xOffset) = i /% n
      val (yTile, yOffset) = j /% m
      val value            = grid(xOffset)(yOffset) + xTile + yTile
      Option.when(value < 10)(value).getOrElse(value % 10 + 1)
    }
  }
}
