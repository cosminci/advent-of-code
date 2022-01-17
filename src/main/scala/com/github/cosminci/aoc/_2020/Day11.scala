package com.github.cosminci.aoc._2020

import com.github.cosminci.aoc.utils

import scala.annotation.tailrec

object Day11 {

  def main(args: Array[String]): Unit = {
    val grid   = utils.loadInputAsStrings("2020/day11.txt")
    val (m, n) = (grid.length - 1, grid.head.length - 1)
    println(s"Part 1: ${countOccupiedAfterStabilizing(grid, m, n, 4, countNeighbours)}")
    println(s"Part 2: ${countOccupiedAfterStabilizing(grid, m, n, 5, countOccupiedDirections)}")
  }

  type CountFn = (Seq[String], Int, Int, Int, Int) => Int

  def countOccupiedAfterStabilizing(grid: Seq[String], m: Int, n: Int, threshold: Int, countFn: CountFn): Int =
    updateUntilStable(grid, m, n, threshold, countFn).map(_.count(_ == '#')).sum

  @tailrec
  private def updateUntilStable(grid: Seq[String], m: Int, n: Int, threshold: Int, countFn: CountFn): Seq[String] = {
    def updateSeats(): Seq[String] =
      (0 to m).map(r =>
        (0 to n).map { c =>
          val neighbours = countFn(grid, m, n, r, c)
          if (grid(r)(c) == 'L' && neighbours == 0) '#'
          else if (grid(r)(c) == '#' && neighbours >= threshold) 'L'
          else grid(r)(c)
        }.mkString
      )

    val newGrid = updateSeats()
    if (newGrid == grid) grid else updateUntilStable(newGrid, m, n, threshold, countFn)
  }

  private val countNeighbours: CountFn = (grid, m, n, row, col) =>
    utils.neighbours(n, m, row, col, includeDiagonals = true).count { case (r, c) => grid(r)(c) == '#' }

  private val directions = Seq((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))
  private val countOccupiedDirections: CountFn = (grid, _, _, row, col) => {
    @tailrec
    def isOccupied(x0: Int, y0: Int, dx: Int, dy: Int): Boolean = {
      val (x, y) = (x0 + dx, y0 + dy)
      if (!grid.isDefinedAt(x) || !grid(x).isDefinedAt(y)) false
      else if (grid(x)(y) == 'L') false
      else if (grid(x)(y) == '#') true
      else isOccupied(x, y, dx, dy)
    }

    directions.count { case (dx, dy) => isOccupied(row, col, dx, dy) }
  }
}
