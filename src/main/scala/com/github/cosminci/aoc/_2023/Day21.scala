package com.github.cosminci.aoc._2023

import com.github.cosminci.aoc.utils

import scala.math.floorMod

object Day21 {

  def main(args: Array[String]): Unit = {
    val grid = utils.loadInputAsStrings("2023/day21.txt")

    println(s"Part 1: ${reachablePlotsFiniteGrid(grid, steps = 64)}")
    println(s"Part 2: ${reachablePlotsInfiniteGrid(grid, steps = 26501365)}")
  }

  final case class Pos(r: Int, c: Int)

  def reachablePlotsFiniteGrid(grid: Seq[String], steps: Int): Int =
    (1 to steps)
      .foldLeft(Set(findStart(grid)))((positions, _) => positions.flatMap(neighbours(_, grid)))
      .size

  def reachablePlotsInfiniteGrid(grid: Seq[String], steps: Int): Long = {
    @annotation.tailrec
    def dfs(positions: Set[Pos], i: Int, factors: Seq[Int]): Long =
      factors match {
        case Seq(f1, f2, f3) => solvePolynomial(f1, f2, f3, steps / grid.length)
        case _ =>
          val nextPositions = positions.flatMap(neighbours(_, grid))
          val nextFactors   = if (i % grid.length == steps % grid.length) factors :+ positions.size else factors
          dfs(nextPositions, i + 1, nextFactors)
      }

    dfs(positions = Set(findStart(grid)), i = 0, factors = Seq.empty)
  }

  private def solvePolynomial(f1: Long, f2: Long, f3: Long, x: Long) = {
    val (a, b, c) = (f3 - 2 * f2 + f1, f2 - f1, f1)
    a * (x * (x - 1) / 2) + b * x + c
  }

  private def neighbours(p: Pos, grid: Seq[String]) =
    Seq((p.r - 1, p.c), (p.r + 1, p.c), (p.r, p.c - 1), (p.r, p.c + 1))
      .collect { case (r, c) if grid(floorMod(r, grid.length))(floorMod(c, grid.head.length)) != '#' => Pos(r, c) }

  private def findStart(grid: Seq[String]) =
    grid.indices
      .flatMap(r => grid(r).indices.map(c => Pos(r, c)))
      .find(pos => grid(pos.r)(pos.c) == 'S')
      .get

}
