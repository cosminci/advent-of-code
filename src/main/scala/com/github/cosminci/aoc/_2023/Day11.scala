package com.github.cosminci.aoc._2023

import com.github.cosminci.aoc.utils

object Day11 {

  def main(args: Array[String]): Unit = {
    val universe = utils.loadInputAsStrings("2023/day11.txt")

    println(s"Part 1: ${sumOfShortestPathsBetweenPairs(universe, expFactor = 2)}")
    println(s"Part 2: ${sumOfShortestPathsBetweenPairs(universe, expFactor = 1_000_000)}")
  }

  def sumOfShortestPathsBetweenPairs(universe: Seq[String], expFactor: Long): Long = {
    val emptyRows = universe.indices.filter(universe(_).forall(_ == '.')).toSet
    val emptyCols = universe.head.indices.filter(c => universe.forall(_(c) == '.')).toSet

    def dist(r1: Int, c1: Int, r2: Int, c2: Int) = {
      val emptyRowCount = (r1.min(r2) until r1.max(r2)).count(emptyRows.contains)
      val emptyColCount = (c1.min(c2) until c1.max(c2)).count(emptyCols.contains)
      (r2 - r1).abs + (c2 - c1).abs + (emptyRowCount + emptyColCount) * (expFactor - 1)
    }

    universe.indices
      .flatMap(r => universe(r).indices.map(c => (r, c)))
      .filter { case (r, c) => universe(r)(c) == '#' }
      .combinations(2)
      .map { case Seq((r1, c1), (r2, c2)) => dist(r1, c1, r2, c2) }
      .sum
  }

}
