package com.github.cosminci.aoc._2023

import com.github.cosminci.aoc.utils

object Day14 {

  def main(args: Array[String]): Unit = {
    val grid = utils.loadInputAsStrings("2023/day14.txt").map(_.toCharArray.toSeq)

    println(s"Part 1: ${loadAfterOneTilt(grid)}")
    println(s"Part 2: ${loadAfterSpins(grid, totalSpins = 1_000_000_000)}")
  }

  def loadAfterOneTilt(grid: Seq[Seq[Char]]): Int =
    rotateCCW(grid).map(slideRocksLeft).map(computeLoad).sum

  def loadAfterSpins(grid: Seq[Seq[Char]], totalSpins: Int): Int = {
    @annotation.tailrec
    def dfs(grid: Seq[Seq[Char]], j: Int, mem: Map[Seq[Seq[Char]], Int]): Seq[Seq[Char]] =
      mem.get(grid) match {
        case None    => dfs(spin(grid), j + 1, mem.updated(grid, j))
        case Some(i) => (1 to (totalSpins - j) % (j - i)).foldLeft(grid)((grid, _) => spin(grid))
      }
    dfs(rotateCCW(grid), j = 0, mem = Map.empty).map(computeLoad).sum
  }

  private def spin(grid: Seq[Seq[Char]]) =
    (1 to 4).foldLeft(grid)((grid, _) => rotateCW(grid.map(slideRocksLeft)))

  private def rotateCCW(grid: Seq[Seq[Char]]): Seq[Seq[Char]] = grid.transpose.reverse

  private def rotateCW(grid: Seq[Seq[Char]]): Seq[Seq[Char]] = grid.transpose.map(_.reverse)

  private def slideRocksLeft(row: Seq[Char]): Seq[Char] = {
    @annotation.tailrec
    def dfs(i: Int, j: Int, row: IndexedSeq[Char]): Seq[Char] =
      if (j == row.length) row
      else if (row(j) == '#') dfs(j + 1, j + 1, row)
      else if (row(i) == '#') dfs(i + 1, j, row)
      else if (row(j) == 'O') dfs(i + 1, j + 1, row.updated(i, row(j)).updated(j, row(i)))
      else dfs(i, j + 1, row)

    dfs(i = 0, j = 0, row.toIndexedSeq)
  }

  private def computeLoad(rocks: Seq[Char]): Int =
    rocks.zipWithIndex.collect { case ('O', i) => rocks.length - i }.sum

}
