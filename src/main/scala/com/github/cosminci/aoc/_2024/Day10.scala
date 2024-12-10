package com.github.cosminci.aoc._2024

import com.github.cosminci.aoc.utils

object Day10 {

  def main(args: Array[String]): Unit = {
    val grid = utils.loadInputAsStrings("2024/day10.txt")

    println(s"Part 1: ${trailheadScoreSum(grid)}")
    println(s"Part 2: ${trailheadRatingSum(grid)}")
  }

  final case class Pos(r: Int, c: Int)

  def trailheadScoreSum(grid: Seq[String]): Int = {
    def dfs(curr: Pos): Seq[Pos] =
      if (grid(curr.r)(curr.c) == '9') Seq(curr)
      else neighbours(curr, grid).flatMap(dfs).distinct

    findTrailheads(grid).map(dfs(_).size).sum
  }

  def trailheadRatingSum(grid: Seq[String]): Int = {
    def dfs(curr: Pos): Int =
      if (grid(curr.r)(curr.c) == '9') 1
      else neighbours(curr, grid).map(dfs).sum

    findTrailheads(grid).map(dfs).sum
  }

  private def neighbours(p: Pos, grid: Seq[String]) =
    Seq((-1, 0), (1, 0), (0, -1), (0, 1))
      .map { case (dr, dc) => (p.r + dr, p.c + dc) }
      .filter { case (r, c) => r.min(c) >= 0 && r.max(c) < grid.length }
      .collect { case (r, c) if grid(r)(c) == grid(p.r)(p.c) + 1 => Pos(r, c) }

  private def findTrailheads(grid: Seq[String]) =
    for {
      r <- grid.indices
      c <- grid(r).indices
      if grid(r)(c) == '0'
    } yield Pos(r, c)

}
