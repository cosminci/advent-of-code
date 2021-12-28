package com.github.cosminci.aoc._2020

import com.github.cosminci.aoc.utils

object Day3 {

  def main(args: Array[String]): Unit = {
    val grid = utils.loadInputAsStrings("2020/day3.txt")

    val slopes = Seq((3, 1), (1, 1), (5, 1), (7, 1), (1, 2))
    println(s"Part 1: ${countTreesForSlope(grid, dy = slopes.head._1, dx = slopes.head._2)}")
    println(s"Part 2: ${slopes.map { case (dy, dx) => countTreesForSlope(grid, dy, dx) }.product }")
  }

  def countTreesForSlope(grid: Seq[String], dy: Int, dx: Int): Long =
    Iterator
      .iterate(0)(_ + dx)
      .takeWhile(_ < grid.length)
      .zipWithIndex
      .count { case (row, idx) => grid(row)((dy * idx) % grid.head.length) == '#' }
}
