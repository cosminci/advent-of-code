package com.github.cosminci.aoc._2020

import cats.syntax.all._
import com.github.cosminci.aoc.utils

import scala.annotation.tailrec

object Day9 {

  def main(args: Array[String]): Unit = {
    val nums = utils.loadInputAsLongs("2020/day9.txt")

    val invalidNum = (findInvalidNumber _).tupled(nums.splitAt(25))
    println(s"Part 1: $invalidNum")
    println(s"Part 2: ${findEncryptionWeakness(invalidNum, nums)}")
  }

  def findInvalidNumber(preamble: Seq[Long], tail: Seq[Long]): Long =
    tail.foldM(preamble) { case (preamble, number) =>
      if (preamble.combinations(2).exists(_.sum == number)) (preamble.tail :+ number).asRight
      else number.asLeft
    }.left.getOrElse(Long.MaxValue)

  def findEncryptionWeakness(target: Long, nums: Seq[Long]): Long = {
    @tailrec
    def dfs(low: Int, high: Int, sum: Long): Seq[Long] =
      if (sum > target) dfs(low + 1, high, sum - nums(low))
      else if (sum < target) dfs(low, high + 1, sum + nums(high + 1))
      else (low to high).map(nums)

    val rangeNums = dfs(low = 0, high = 0, sum = nums.head)
    rangeNums.min + rangeNums.max
  }
}
