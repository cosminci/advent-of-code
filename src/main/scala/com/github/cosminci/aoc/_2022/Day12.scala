package com.github.cosminci.aoc._2022

import com.github.cosminci.aoc.utils

import scala.util.chaining._

object Day12 {

  def main(args: Array[String]): Unit = {
    val grid = utils.loadInputAsStrings("2022/day12.txt").map(_.toArray).toArray

    val start = findPosition(grid, target = 'S').tap { case (x, y) => grid(x)(y) = 'a' }
    val end   = findPosition(grid, target = 'E').tap { case (x, y) => grid(x)(y) = 'z' }

    println(s"Part 1: ${fewestStepsToReachEnd(grid, start, isEnd = _ == end, isValidNei = maxOneStepUp(grid))}")
    println(s"Part 2: ${fewestStepsToReachEnd(grid, end, isEnd = isGround(grid), isValidNei = maxOneStepDown(grid))}")
  }

  type Pos          = (Int, Int)
  type NeiPredicate = (Pos, Pos) => Boolean
  type EndPredicate = Pos => Boolean

  def fewestStepsToReachEnd(grid: Array[Array[Char]], start: Pos, isEnd: EndPredicate, isValidNei: NeiPredicate): Int =
    Iterator
      .iterate((Seq((start, 0)), Set(start))) { case (unseen, seen) =>
        val (curr @ (x, y), steps) = unseen.head
        utils
          .neighbours(m = grid.length - 1, n = grid.head.length - 1, x, y)
          .filter(nei => !seen.contains(nei) && isValidNei(curr, nei))
          .foldLeft(unseen.tail, seen) { case ((unseen, seen), nei) => (unseen :+ (nei, steps + 1), seen + nei) }
      }
      .dropWhile { case ((pos, _) +: _, _) => !isEnd(pos) }
      .next()
      .pipe { case ((_, steps) +: _, _) => steps }

  private def isGround(grid: Array[Array[Char]])(pos: Pos) =
    pos match { case (x, y) => grid(x)(y) == 'a' }

  private def maxOneStepUp(grid: Array[Array[Char]])(p1: Pos, p2: Pos): Boolean =
    (p1, p2) match { case ((x0, y0), (x1, y1)) => grid(x1)(y1) - grid(x0)(y0) <= 1 }

  private def maxOneStepDown(grid: Array[Array[Char]])(p1: Pos, p2: Pos): Boolean =
    maxOneStepUp(grid)(p2, p1)

  private def findPosition(grid: Array[Array[Char]], target: Char) =
    grid.indices.flatMap { x =>
      grid(x).indices.collectFirst { case y if grid(x)(y) == target => (x, y) }
    }.head

}
