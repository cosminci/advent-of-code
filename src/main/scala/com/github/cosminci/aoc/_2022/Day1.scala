package com.github.cosminci.aoc._2022

import com.github.cosminci.aoc.utils

import scala.annotation.tailrec
import scala.util._

object Day1 {

  def main(args: Array[String]): Unit = {
    val input = utils.loadInputAsStrings("2022/day1.txt")

    println(s"Part 1: ${mostCalories(input)}")
    println(s"Part 2: ${topThreeCaloriesSum(input)}")
  }

  def mostCalories(input: Seq[String]): Int =
    calorieBatchSizes(input).max

  def topThreeCaloriesSum(input: Seq[String]): Int =
    calorieBatchSizes(input).sorted.takeRight(3).sum

  private def calorieBatchSizes(input: Seq[String]) = {
    @tailrec
    def dfs(i: Int, runningSum: Int, bags: Seq[Int]): Seq[Int] =
      if (i == input.length) bags :+ runningSum
      else if (input(i).isEmpty) dfs(i + 1, runningSum = 0, bags :+ runningSum)
      else dfs(i + 1, runningSum + input(i).toInt, bags)

    dfs(i = 0, runningSum = 0, bags = Seq.empty)
  }

}
