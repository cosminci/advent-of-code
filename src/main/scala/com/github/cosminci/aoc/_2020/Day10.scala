package com.github.cosminci.aoc._2020

import com.github.cosminci.aoc.utils

object Day10 {

  def main(args: Array[String]): Unit = {
    val adapters = utils.loadInputAsInts("2020/day10.txt")
    println(s"Part 1: ${multiply1And3Diffs(adapters)}")
    println(s"Part 1: ${countWaysToArrange(adapters)}")
  }

  def multiply1And3Diffs(adapters: Seq[Int]): Int = {
    val (oneDiffs, threeDiffs, _) = (0 +: adapters.sorted).foldRight((0, 0, adapters.max + 3)) {
      case (n, (oneDiffCount, threeDiffCount, prevN)) =>
        val difference        = prevN - n
        val newOneDiffCount   = oneDiffCount + Option.when(difference == 1)(1).getOrElse(0)
        val newThreeDiffCount = threeDiffCount + Option.when(difference == 3)(1).getOrElse(0)
        (newOneDiffCount, newThreeDiffCount, n)
    }
    oneDiffs * threeDiffs
  }

  def countWaysToArrange(adapters: Seq[Int]): Long =
    adapters.sorted
      .foldLeft(Map(0 -> 1L)) { case (counts, n) =>
        val nCount = counts.getOrElse(n - 1, 0L) + counts.getOrElse(n - 2, 0L) + counts.getOrElse(n - 3, 0L)
        counts + (n -> nCount)
      }(adapters.max)
}
