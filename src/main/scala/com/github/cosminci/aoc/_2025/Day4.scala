package com.github.cosminci.aoc._2025

import com.github.cosminci.aoc.utils._

object Day4 {

  def main(args: Array[String]): Unit = {
    val input = loadInputAsStrings("2025/day4.txt")

    println(s"Part 1: ${countRemovableRollsSinglePass(input)}")
    println(s"Part 2: ${countRemovableRollsUntilExhausted(input)}")
  }

  def countRemovableRollsSinglePass(input: Seq[String]): Int =
    findRemovableRolls(input).size

  def countRemovableRollsUntilExhausted(input: Seq[String]): Int = {
    @annotation.tailrec
    def dfs(removed: Set[(Int, Int)]): Set[(Int, Int)] =
      findRemovableRolls(input, removed) match {
        case Seq()    => removed
        case toRemove => dfs(removed ++ toRemove)
      }
    dfs(removed = Set.empty).size
  }

  private def findRemovableRolls(input: Seq[String], removed: Set[(Int, Int)] = Set.empty) = {
    val findNeiFn = neighbours(input.length - 1, input.head.length - 1, _, _, includeDiagonals = true)
    def isRollPresent(r: Int, c: Int) = input(r)(c) == '@' && !removed.contains((r, c))
    input.indices.flatMap(r => input(r).indices.map(c => (r, c))).filter { case (r, c) =>
      isRollPresent(r, c) && findNeiFn(r, c).count { case (nr, nc) => isRollPresent(nr, nc) } < 4
    }
  }

}
