package com.github.cosminci.aoc._2021

import com.github.cosminci.aoc.utils

object Day3 {
  def main(args: Array[String]): Unit = {
    val binaryNums = utils.loadInputAsStrings("2021/day3.txt")
    println(powerConsumption(binaryNums))
    println(lifeSupport(binaryNums))
  }

  def powerConsumption(binaryNums: Seq[String]): Int = {
    val (epsilonRate, gammaRate) =
      mostAndLeastSignificantBits(binaryNums).foldLeft("", "") {
        case ((epsilonRate, gammaRate), Seq(epsilonBit, gammaBit)) =>
          (epsilonRate :+ epsilonBit, gammaRate :+ gammaBit)
      }
    Integer.parseInt(epsilonRate, 2) * Integer.parseInt(gammaRate, 2)
  }

  def lifeSupport(binaryNums: Seq[String]): Int = {
    def oxygenRating(nums: Seq[String], idx: Int): String = {
      val msb = mostAndLeastSignificantBits(nums).map(_.last)
      val remaining = nums.filter(n => n(idx) == msb(idx))
      Option.when(remaining.length == 1)(remaining.head).getOrElse(oxygenRating(remaining, idx + 1))
    }

    def co2Scrubber(nums: Seq[String], idx: Int): String = {
      val lsb = mostAndLeastSignificantBits(nums).map(_.head)
      val remaining = nums.filter(n => n(idx) == lsb(idx))
      Option.when(remaining.length == 1)(remaining.head).getOrElse(co2Scrubber(remaining, idx + 1))
    }

    Integer.parseInt(oxygenRating(binaryNums, 0), 2) *
      Integer.parseInt(co2Scrubber(binaryNums, 0), 2)
  }

  private def mostAndLeastSignificantBits(binaryNums: Seq[String]) =
    binaryNums.head.indices
      .map { i =>
        binaryNums
          .map(_.charAt(i))
          .groupMapReduce(identity)(_ => 1)(_ + _)
          .toSeq
          .sortBy { case (bit, count) => (count, bit) }
          .map { case (bit, _) => bit }
      }
}
