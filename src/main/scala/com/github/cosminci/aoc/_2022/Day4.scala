package com.github.cosminci.aoc._2022

import com.github.cosminci.aoc.utils

object Day4 {

  def main(args: Array[String]): Unit = {
    val input = utils.loadInputAsStrings("2022/day4.txt")
    val pairs = parseInput(input)

    println(s"Part 1: ${pairs.count(isCompleteOverlap.tupled)}")
    println(s"Part 2: ${pairs.count(isPartialOverlap.tupled)}")
  }

  val isCompleteOverlap: (Range, Range) => Boolean =
    (i1, i2) => Seq(i1, i2).contains(i1.intersect(i2))

  val isPartialOverlap: (Range, Range) => Boolean =
    (i1, i2) => i1.intersect(i2).nonEmpty

  private def parseInput(input: Seq[String]) = {
    val pattern = "([0-9]+)-([0-9]+),([0-9]+)-([0-9]+)".r
    input.map { case pattern(x1, y1, x2, y2) => (x1.toInt to y1.toInt, x2.toInt to y2.toInt) }
  }
}
