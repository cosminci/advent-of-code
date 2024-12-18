package com.github.cosminci.aoc._2024

import com.github.cosminci.aoc.utils

import scala.util.chaining.scalaUtilChainingOps

object Day18 {

  def main(args: Array[String]): Unit = {
    val bytes = parseInput(utils.loadInputAsStrings("2024/day18.txt"))

    println(s"Part 1: ${minStepsAfterTime(bytes.take(1024).toSet)}")
    println(s"Part 2: ${firstBlockingByte(bytes).pipe(p => s"${p.c},${p.r}")}")
  }

  private val gridSize = 71
  final case class Pos(r: Int, c: Int)

  def minStepsAfterTime(corrupted: Set[Pos]): Int = {
    @annotation.tailrec
    def dfs(toVisit: Set[Pos], visited: Set[Pos], steps: Int): Int =
      if (toVisit.isEmpty) Int.MaxValue
      else if (toVisit.contains(Pos(gridSize - 1, gridSize - 1))) steps
      else {
        val nextToVisit = toVisit.flatMap(neighbours).filterNot(p => visited.contains(p) || corrupted.contains(p))
        dfs(nextToVisit, visited ++ nextToVisit, steps + 1)
      }
    val start = Pos(r = 0, c = 0)
    dfs(toVisit = Set(start), visited = Set(start), steps = 0)
  }

  def firstBlockingByte(bytes: Seq[Pos]): Pos = {
    val corruptedOverTime = bytes.scanLeft(Set.empty[Pos])(_ + _)
    @annotation.tailrec
    def binarySearch(left: Int, right: Int): Int =
      if (left >= right) left - 1
      else {
        val mid = left + (right - left) / 2
        if (minStepsAfterTime(corruptedOverTime(mid)) == Int.MaxValue) binarySearch(left, mid)
        else binarySearch(mid + 1, right)
      }
    bytes(binarySearch(left = 0, right = bytes.length))
  }

  private def neighbours(pos: Pos): Set[Pos] =
    Seq((0, -1), (-1, 0), (0, 1), (1, 0))
      .map { case (dr, dc) => Pos(pos.r + dr, pos.c + dc) }
      .filter { case Pos(r, c) => r.min(c) >= 0 && r.max(c) < gridSize }
      .toSet

  private def parseInput(lines: Seq[String]) =
    lines.map { case s"$c,$r" => Pos(r.toInt, c.toInt) }

}
