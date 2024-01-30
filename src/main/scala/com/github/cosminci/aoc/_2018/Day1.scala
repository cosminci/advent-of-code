package com.github.cosminci.aoc._2018

import com.github.cosminci.aoc.utils

object Day1 {

  def main(args: Array[String]): Unit = {
    val frequencies = utils.loadInputAsInts("2018/day1.txt")

    println(s"Part 1: ${finalFrequency(frequencies)}")
    println(s"Part 2: ${firstDuplicateFrequency(frequencies)}")
  }

  def finalFrequency(frequencies: Seq[Int]): Int =
    frequencies.sum

  def firstDuplicateFrequency(frequencies: Seq[Int]): Int = {
    @annotation.tailrec
    def dfs(i: Int, runningSum: Int, prevSums: Set[Int]): Int =
      if (prevSums.contains(runningSum)) runningSum
      else dfs(i + 1, runningSum + frequencies(i % frequencies.size), prevSums + runningSum)

    dfs(i = 0, runningSum = 0, prevSums = Set.empty)
  }

}
