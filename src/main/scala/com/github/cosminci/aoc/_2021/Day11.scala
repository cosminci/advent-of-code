package com.github.cosminci.aoc._2021

import com.github.cosminci.aoc.utils

import scala.annotation.tailrec

object Day11 {
  def main(args: Array[String]): Unit = {
    val gridSeq = utils.loadInputAsStrings("2021/day11.txt").map(_.toCharArray.map(_ - '0'))
    val (m, n)  = (gridSeq.length, gridSeq.head.length)
    val gridMap = (0 until m).flatMap(r => (0 until n).map(c => (r, c) -> gridSeq(r)(c))).toMap

    println(s"Part I: ${countTotalFlashes(gridMap, m, n, days = 100)}")
    println(s"Part II: ${judgementDay(gridMap, m, n)}")
  }

  type State = (Map[(Int, Int), Int], Set[(Int, Int)])

  def countTotalFlashes(grid: Map[(Int, Int), Int], m: Int, n: Int, days: Int): Int =
    (1 to days).foldLeft(grid, 0) { case ((prevGrid, count), _) =>
      val (grid, flashed) = countDayFlashes(prevGrid, m, n)
      (grid, count + flashed.size)
    }._2

  def judgementDay(grid: Map[(Int, Int), Int], m: Int, n: Int): Int = {
    @tailrec
    def dfs(prevGrid: Map[(Int, Int), Int], day: Int): Int = {
      val (grid, flashed) = countDayFlashes(prevGrid, m, n)
      if (flashed.size == m * n) day else dfs(grid, day + 1)
    }
    dfs(grid, day = 1)
  }

  private def countDayFlashes(prevGrid: Map[(Int, Int), Int], m: Int, n: Int): State = {
    val grid    = prevGrid.map { case (k, v) => (k, v + 1) }
    val toVisit = prevGrid.keys.filter { case (x, y) => grid((x, y)) == 10 }.toSeq
    val visited = Set.from(toVisit)

    def flash(state: State, octopus: (Int, Int)): State = {
      val ((grid, visited), (x, y)) = (state, octopus)
      utils.neighbours(m - 1, n - 1, x, y, includeDiagonals = true).foldLeft(grid.updated((x, y), 0), visited) {
        case ((grid, visited), (x, y)) =>
          if (visited.contains((x, y)))
            (grid, visited)
          else {
            val newGrid = grid.updated((x, y), grid((x, y)) + 1)
            if (newGrid((x, y)) == 10) flash((newGrid, visited + ((x, y))), (x, y))
            else (newGrid, visited)
          }
      }
    }

    toVisit.foldLeft(grid, visited)(flash)
  }
}
