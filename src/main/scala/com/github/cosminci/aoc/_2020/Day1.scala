package com.github.cosminci.aoc._2020

import com.github.cosminci.aoc.utils

object Day1 {

  def main(args: Array[String]): Unit = {
    val input = utils.loadInputAsInts("2020/day1.txt")

    println(s"Part 1: ${multiplyTwoEntriesThatMatchSum(input, 2020)}")
    println(s"Part 2: ${multiplyThreeEntriesThatMatchSum(input, 2020)}")
  }

  def multiplyTwoEntriesThatMatchSum(input: Seq[Int], targetSum: Int): Option[Int] = {
    val nums = input.toSet
    input.collectFirst {
      case n if nums.contains(targetSum - n) =>
        n * (targetSum - n)
    }
  }

  def multiplyThreeEntriesThatMatchSum(input: Seq[Int], targetSum: Int): Option[Int] = {
    val sums = input.combinations(2).map(c => c.sum -> c).toMap
    input.collectFirst {
      case n if sums.contains(targetSum - n) =>
        (sums(targetSum - n) :+ n).product
    }
  }

}
