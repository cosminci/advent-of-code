package com.github.cosminci.aoc._2021

import com.github.cosminci.aoc.utils

import scala.annotation.tailrec
import scala.collection.immutable.TreeSet
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
    val (n, m) = (grid.length - 1, grid.head.length - 1)

    @tailrec
    def dfs(toVisit: TreeSet[(Int, Int, Int)], visited: Set[(Int, Int)]): Int = {
      val curr @ (risk, x, y) = toVisit.last
      if (x == n && y == m) return -risk

      val (newToVisit, newVisited) = utils.neighbours(n, m, x, y).foldLeft(toVisit, visited) {
        case ((toVisit, visited), (x, y)) =>
          if (visited.contains((x, y))) (toVisit, visited)
          else (toVisit + ((risk - grid(x)(y), x, y)), visited + ((x, y)))
      }
      dfs(newToVisit - curr, newVisited)
    }

    dfs(toVisit = TreeSet((0, 0, 0)), visited = Set((0, 0)))
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
