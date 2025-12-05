package com.github.cosminci.aoc._2025

import com.github.cosminci.aoc.utils.loadInputAsStrings

object Day5 {

  def main(args: Array[String]): Unit = {
    val (freshRanges, idsToCheck) = parseInput(loadInputAsStrings("2025/day5.txt"))

    println(s"Part 1: ${countFreshIDs(freshRanges, idsToCheck)}")
    println(s"Part 2: ${countAllPossibleFreshIDs(freshRanges.sortBy(_.l))}")
  }

  final case class Interval(l: Long, r: Long)

  def countFreshIDs(ranges: Seq[Interval], ids: Seq[Long]): Long =
    ids.count(id => ranges.exists { case Interval(l, r) => id >= l && id <= r })

  def countAllPossibleFreshIDs(ranges: Seq[Interval]): Long = {
    @annotation.tailrec
    def findEnd(i: Int, maxR: Long): (Int, Long) =
      if (!ranges.isDefinedAt(i + 1) || maxR < ranges(i + 1).l) (i, maxR)
      else findEnd(i + 1, maxR.max(ranges(i + 1).r))

    @annotation.tailrec
    def merge(mergedRanges: Seq[Interval], i: Int): Seq[Interval] =
      if (i == ranges.length) mergedRanges
      else {
        val (j, maxR) = findEnd(i, ranges(i).r)
        merge(mergedRanges :+ Interval(ranges(i).l, maxR), j + 1)
      }

    merge(mergedRanges = Seq.empty, i = 0).map { case Interval(l, r) => r - l + 1 }.sum
  }

  private def parseInput(lines: Seq[String]) = {
    val (rangeLines, idLines) = lines.splitAt(lines.indexWhere(_.isEmpty))
    val freshRanges           = rangeLines.map(_.split('-').map(_.toLong)).map { case Array(a, b) => Interval(a, b) }
    val idsToCheck            = idLines.tail.map(_.toLong)
    (freshRanges, idsToCheck)
  }

}
