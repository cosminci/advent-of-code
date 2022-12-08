package com.github.cosminci.aoc._2022

import com.github.cosminci.aoc.utils

object Day8 {

  def main(args: Array[String]): Unit = {
    val input = utils.loadInputAsStrings("2022/day8.txt").map(_.map(_ - '0'))

    val (m, n)      = (input.length, input.head.length)
    val lineOfSight = computeLineOfSightDistances(m, n, input).toMap

    println(s"Part 1: ${countVisibleTrees(m, n, lineOfSight)}")
    println(s"Part 2: ${highestScenicScore(m, n, lineOfSight)}")
  }

  type Coordinate = (Int, Int)
  type Direction  = (Int, Int)

  def countVisibleTrees(m: Int, n: Int, lineOfSight: Map[Coordinate, Map[Direction, Int]]): Int =
    lineOfSight.count { case ((x, y), countsPerDirection) =>
      countsPerDirection.exists {
        case ((-1, 0), count) => x == count
        case ((1, 0), count)  => x == m - 1 - count
        case ((0, -1), count) => y == count
        case ((0, 1), count)  => y == n - 1 - count
      }
    }

  def highestScenicScore(m: Int, n: Int, lineOfSight: Map[Coordinate, Map[Direction, Int]]): Int =
    lineOfSight.map { case ((x, y), countsPerDirection) =>
      countsPerDirection.map {
        case ((-1, 0), count) => if (x == count) count else count + 1
        case ((1, 0), count)  => if (x + count == m - 1) count else count + 1
        case ((0, -1), count) => if (y == count) count else count + 1
        case ((0, 1), count)  => if (y + count == n - 1) count else count + 1
      }.product
    }.max

  private def computeLineOfSightDistances(m: Int, n: Int, input: Seq[Seq[Int]]) =
    for {
      x0 <- 0 until m
      y0 <- 0 until n
    } yield (x0, y0) -> Seq((-1, 0), (1, 0), (0, -1), (0, 1)).map { case (dx, dy) =>
      (dx, dy) -> Iterator
        .iterate((x0 + dx, y0 + dy)) { case (x, y) => (x + dx, y + dy) }
        .takeWhile { case (x, y) => x >= 0 && y >= 0 && x < m && y < n && input(x)(y) < input(x0)(y0) }
        .length
    }.toMap

}
