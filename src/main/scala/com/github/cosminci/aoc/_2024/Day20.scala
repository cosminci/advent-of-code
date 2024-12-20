package com.github.cosminci.aoc._2024

import com.github.cosminci.aoc.utils

import scala.collection.immutable.TreeSet
import scala.collection.mutable

object Day20 {

  def main(args: Array[String]): Unit = {
    val grid = utils.loadInputAsStrings("2024/day20.txt")

    println(s"Part 1: ${cheatsThatSaveMin100Picos(grid, maxCheatDistance = 2)}")
    println(s"Part 2: ${cheatsThatSaveMin100Picos(grid, maxCheatDistance = 20)}")
  }

  final case class Pos(r: Int, c: Int)
  final case class State(pos: Pos, cheatsLeft: Int, nanos: Int)

  def cheatsThatSaveMin100Picos(grid: Seq[String], maxCheatDistance: Int): Int = {
    val distances = computePairwiseDistances(grid)
    distances.map { case (cheatStart, distanceToEnd) =>
      neighbours(cheatStart, grid, distance = maxCheatDistance).count { cheatEnd =>
        val cheatDistance = (cheatEnd.r - cheatStart.r).abs + (cheatEnd.c - cheatStart.c).abs
        distances(cheatEnd) - distanceToEnd - cheatDistance >= 100
      }
    }.sum
  }

  private def computePairwiseDistances(grid: Seq[String]): Map[Pos, Int] = {
    @annotation.tailrec
    def dfs(curr: Pos, pairwiseDistances: Map[Pos, Int]): Map[Pos, Int] =
      neighbours(curr, grid, distance = 1).find(next => !pairwiseDistances.contains(next)) match {
        case None       => pairwiseDistances
        case Some(next) => dfs(next, pairwiseDistances + (next -> (pairwiseDistances(curr) + 1)))
      }
    val end = findEnd(grid)
    dfs(end, pairwiseDistances = Map(end -> 0))
  }

  private def neighbours(curr: Pos, grid: Seq[String], distance: Int) = {
    val candidates = for {
      dy <- -distance to distance
      dx <- -distance + dy.abs to distance - dy.abs
    } yield Pos(curr.r + dy, curr.c + dx)

    candidates.filter { case Pos(r, c) => r.min(c) >= 0 && r.max(c) < grid.length && grid(r)(c) != '#' }
  }

  private def findEnd(grid: Seq[String]) =
    grid.indices.flatMap(r => grid(r).indices.map(c => (r, c)))
      .collectFirst { case (r, c) if grid(r)(c) == 'E' => Pos(r, c) }.get

}
