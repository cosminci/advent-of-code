package com.github.cosminci.aoc._2024

import com.github.cosminci.aoc.utils

import scala.util.chaining.scalaUtilChainingOps

object Day12 {

  def main(args: Array[String]): Unit = {
    val grid = utils.loadInputAsStrings("2024/day12.txt")

    println(s"Part 1: ${totalFencingPrice(grid, priceFn = perimeter)}")
    println(s"Part 2: ${totalFencingPrice(grid, priceFn = sideCount)}")
  }

  final case class Pos(r: Int, c: Int)

  def totalFencingPrice(grid: Seq[String], priceFn: Set[Pos] => Int): Int =
    indices(grid).foldLeft(Set.empty[Pos], Set.empty[Set[Pos]]) {
      case ((visited, plots), pos) =>
        if (visited.contains(pos)) (visited, plots)
        else discoverPlot(pos, grid, visited).pipe(p => (visited ++ p, plots + p))
      }.pipe { case (_, plots) => plots.toSeq.map(p => p.size * priceFn(p)).sum }

  private def discoverPlot(pos: Pos, grid: Seq[String], visited: Set[Pos]) = {
    @annotation.tailrec
    def bfs(curr: Set[Pos], plot: Set[Pos]): Set[Pos] =
      if (curr.isEmpty) plot
      else {
        val next = curr.flatMap(validNeighbours(_, grid).diff(plot).diff(visited))
        bfs(next, plot ++ next)
      }
    bfs(curr = Set(pos), plot = Set(pos))
  }

  private def perimeter(plot: Set[Pos]) =
    plot.toSeq.map(pos => 4 - neighbours(pos).count(plot.contains)).sum

  private def sideCount(plot: Set[Pos]) = {
    def cornerCount(p: Pos) =
      (dirs :+ dirs.head).sliding(2).count { case Seq((dr1, dc1), (dr2, dc2)) =>
        val plotContainsOrthogonal1 = plot.contains(Pos(p.r + dr1, p.c + dc1))
        val plotContainsOrthogonal2 = plot.contains(Pos(p.r + dr2, p.c + dc2))
        val plotContainsDiagonal    = plot.contains(Pos(p.r + dr1 + dr2, p.c + dc1 + dc2))
        val isExteriorCorner        = !plotContainsOrthogonal1 && !plotContainsOrthogonal2
        val isInteriorCorner        = plotContainsOrthogonal1 && plotContainsOrthogonal2 && !plotContainsDiagonal
        isExteriorCorner || isInteriorCorner
      }
    plot.toSeq.map(cornerCount).sum
  }

  private def validNeighbours(p: Pos, grid: Seq[String]) =
    neighbours(p).filter(n => withinBounds(n, grid) && grid(n.r)(n.c) == grid(p.r)(p.c))

  private def withinBounds(p: Pos, grid: Seq[String]) =
    p.r.min(p.c) >= 0 && p.r.max(p.c) < grid.length

  private val dirs               = Seq((0, -1), (-1, 0), (0, 1), (1, 0))
  private def neighbours(p: Pos) = dirs.map { case (dr, dc) => Pos(p.r + dr, p.c + dc) }.toSet

  private def indices(grid: Seq[String]) =
    for {
      r <- grid.indices
      c <- grid(r).indices
    } yield Pos(r, c)

}
