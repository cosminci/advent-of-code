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
      leastAndMostFrequentBits(binaryNums).foldLeft("", "") {
        case ((epsilonRate, gammaRate), Seq(epsilonBit, gammaBit)) =>
          (epsilonRate :+ epsilonBit, gammaRate :+ gammaBit)
      }
    Integer.parseInt(epsilonRate, 2) * Integer.parseInt(gammaRate, 2)
  }

  def lifeSupport(binaryNums: Seq[String]): Int = {
    @annotation.tailrec
    def dfs(nums: Seq[String], idx: Int, freqFn: Seq[String] => Seq[Char]): String = {
      val frequentBit = freqFn(nums)
      val remaining   = nums.filter(n => n(idx) == frequentBit(idx))
      if (remaining.length == 1) remaining.head else dfs(remaining, idx + 1, freqFn)
    }

    Integer.parseInt(dfs(binaryNums, 0, nums => leastAndMostFrequentBits(nums).map(_.last)), 2) *
      Integer.parseInt(dfs(binaryNums, 0, nums => leastAndMostFrequentBits(nums).map(_.head)), 2)
  }

  private def leastAndMostFrequentBits(binaryNums: Seq[String]) =
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
