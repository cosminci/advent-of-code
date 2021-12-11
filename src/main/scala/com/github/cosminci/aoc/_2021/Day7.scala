package com.github.cosminci.aoc._2021

import com.github.cosminci.aoc.utils

object Day7 {
  def main(args: Array[String]): Unit = {
    val input = utils.loadInputAsStrings("2021/day7.txt").head.split(',').map(_.toInt)

    println(s"Part I: ${costToAlignToMedian(input)}")
    println(s"Part II: ${costToAlignToMean(input)}")
  }

  def costToAlignToMedian(positions: Seq[Int]): Int = {
    val (sorted, n) = (positions.sorted, positions.length)
    val median      = Option.when(n % 2 == 1)(sorted(n / 2)).getOrElse((sorted(n / 2) + sorted(n / 2 + 1)) / 2)
    positions.foldLeft(0)((cost, p) => cost + math.abs(p - median))
  }

  def costToAlignToMean(positions: Seq[Int]): Int = {
    def gauss(n: Int) = n * (n + 1) / 2
    val mean          = positions.sum / positions.length
    positions.foldLeft(0)((cost, p) => cost + gauss(math.abs(p - mean)))
  }
}
