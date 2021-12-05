package com.github.cosminci.aoc._2021

import com.github.cosminci.aoc.utils

import scala.annotation.tailrec

object Day3 {
  def main(args: Array[String]): Unit = {
    val binaryNums = utils.loadInputAsStrings("2021/day3.txt")
    println(powerConsumption(binaryNums))
    println(lifeSupport(binaryNums))
  }

  def powerConsumption(binaryNums: Seq[String]): Int =
    binaryNums.head.indices
      .foldLeft(Seq(0, 0)) { case (Seq(gammaRate, epsilonRate), idx) =>
        val (gammaBit, epsilonBit) = bitsByFrequency(binaryNums.map(_.charAt(idx)))
        Seq(gammaRate * 2 + gammaBit, epsilonRate * 2 + epsilonBit)
      }.product

  def lifeSupport(binaryNums: Seq[String]): Int = {
    @tailrec
    def dfs(nums: Seq[String], idx: Int, bitPredicateFn: Seq[Char] => Int): String = {
      val remaining = nums.filter(n => n(idx) - '0' == bitPredicateFn(nums.map(_.charAt(idx))))
      if (remaining.length == 1) remaining.head else dfs(remaining, idx + 1, bitPredicateFn)
    }

    Integer.parseInt(dfs(binaryNums, idx = 0, bitsByFrequency(_)._1), 2) *
      Integer.parseInt(dfs(binaryNums, idx = 0, bitsByFrequency(_)._2), 2)
  }

  private def bitsByFrequency(bits: Seq[Char]): (Int, Int) =
    Option.when(bits.count(_ == '1') >= bits.length / 2.0)((1, 0)).getOrElse((0, 1))
}
