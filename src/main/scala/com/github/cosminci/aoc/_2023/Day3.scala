package com.github.cosminci.aoc._2023

import com.github.cosminci.aoc.utils

object Day3 {

  def main(args: Array[String]): Unit = {
    val input = utils.loadInputAsStrings("2023/day3.txt")

    println(s"Part 1: ${enginePartSum(input)}")
    println(s"Part 2: ${gearRatioSum(input)}")
  }

  def enginePartSum(input: Seq[String]): Int =
    input.zipWithIndex.flatMap { case (chars, r) =>
      "\\d+".r.findAllMatchIn(chars).flatMap { partMatch =>
        val symbols = neighbours(r, partMatch.start, partMatch.end - 1, input).map { case (r, c) => input(r)(c) }
        Option.when(symbols.exists(ch => !ch.isDigit && ch != '.'))(partMatch.matched.toInt)
      }
    }.sum

  def gearRatioSum(input: Seq[String]): Int =
    mapGearsToParts(input)
      .groupMap { case ((r, c), _) => (r, c) } { case (_, part) => part }
      .values
      .collect { case parts if parts.size == 2 => parts.product }
      .sum

  private def mapGearsToParts(input: Seq[String]) =
    input.zipWithIndex
      .flatMap { case (chars, r) =>
        "\\d+".r.findAllMatchIn(chars).flatMap { partMatch =>
          val nei = neighbours(r, partMatch.start, partMatch.end - 1, input)
          nei.collect { case (r, c) if input(r)(c) == '*' => (r, c) -> partMatch.matched.toInt }
        }
      }

  private def neighbours(r: Int, c0: Int, c1: Int, input: Seq[String]) = {
    val (m, n) = (input.length, input.head.length)
    val above  = (c0 - 1 to c1 + 1).map(c => (r - 1, c))
    val below  = (c0 - 1 to c1 + 1).map(c => (r + 1, c))
    (above ++ below ++ Seq((r, c0 - 1), (r, c1 + 1)))
      .collect { case (r, c) if r >= 0 && c >= 0 && r < m && c < n => (r, c) }
  }

}
