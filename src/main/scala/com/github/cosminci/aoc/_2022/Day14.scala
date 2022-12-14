package com.github.cosminci.aoc._2022

import com.github.cosminci.aoc.utils

import scala.annotation.tailrec
import scala.util.chaining._

object Day14 {

  def main(args: Array[String]): Unit = {
    val input = utils.loadInputAsStrings("2022/day14.txt")
    val rocks = parseInput(input)

    println(s"Part 1: ${countSandGrainUntilStable(rocks)}")
    println(s"Part 2: ${countSandGrainUntilSourceBlocked(rocks)}")
  }

  final case class Pos(x: Int, y: Int)
  private val source = Pos(x = 500, y = 0)

  def countSandGrainUntilStable(rocks: Set[Pos]): Int = {
    val bottom     = rocks.groupMapReduce(_.x)(_.y)(_ max _)
    def isBlocked  = (p: Pos) => rocks.contains(p)
    def shouldStop = (p: Pos) => bottom.get(p.x).forall(_ < p.y)
    countSandGrainUntil(isBlocked, shouldStop)
  }

  def countSandGrainUntilSourceBlocked(rocks: Set[Pos]): Int = {
    val bottomY    = rocks.map(_.y).max + 2
    def isBlocked  = (p: Pos) => rocks.contains(p) || p.y == bottomY
    def shouldStop = (p: Pos) => p == source
    countSandGrainUntil(isBlocked, shouldStop) + 1
  }

  private def countSandGrainUntil(isBlocked: Pos => Boolean, shouldStop: Pos => Boolean) = {
    @tailrec
    def dfs(restingSand: Set[Pos], prev: Seq[Pos]): Int = {
      val next = nextPosition(prev.last, restingSand, isBlocked)
      if (shouldStop(next)) restingSand.size
      else if (next == prev.last) dfs(restingSand + next, prev.dropRight(1))
      else dfs(restingSand, prev :+ next)
    }
    dfs(restingSand = Set.empty, prev = Seq(source))
  }

  private def nextPosition(curr: Pos, restingSand: Set[Pos], isBlocked: Pos => Boolean): Pos =
    Seq((0, 1), (-1, 1), (1, 1))
      .map { case (dx, dy) => Pos(curr.x + dx, curr.y + dy) }
      .find(p => !restingSand.contains(p) && !isBlocked(p))
      .getOrElse(curr)

  private def parseInput(input: Seq[String]) =
    input.flatMap {
      _.split(" -> ")
        .map(_.split(','))
        .map(tuple => (tuple.head.toInt, tuple.last.toInt))
        .pipe(path => path.sliding(2).flatMap(pathSegments))
    }.toSet

  private def pathSegments(ends: Array[(Int, Int)]) = ends match {
    case Array((x0, y0), (x1, y1)) =>
      if (x0 == x1) (y0 to y1 by (y1 - y0).sign).map(y => Pos(x0, y))
      else (x0 to x1 by (x1 - x0).sign).map(x => Pos(x, y0))
  }

}
