package com.github.cosminci.aoc._2025

import com.github.cosminci.aoc.utils.loadInputAsStrings

object Day3 {

  def main(args: Array[String]): Unit = {
    val batteryBanks = loadInputAsStrings("2025/day3.txt").map(_.map(_ - '0'))

    println(s"Part 1: ${totalJoltageOutput(batteryBanks, numBatteries = 2)}")
    println(s"Part 2: ${totalJoltageOutput(batteryBanks, numBatteries = 12)}")
  }

  def totalJoltageOutput(batteryBanks: Seq[Seq[Int]], numBatteries: Int): Long =
    batteryBanks.map(findMaxJoltage(_, numBatteries)).sum

  private def findMaxJoltage(bank: Seq[Int], numBatteries: Int) = {
    @annotation.tailrec
    def find(picked: Seq[Int]): Long =
      if (picked.size == numBatteries) picked.map(bank).mkString.toLong
      else find(picked :+ (picked.lastOption.getOrElse(-1) + 1 to bank.length - numBatteries + picked.size).maxBy(bank))

    find(picked = Seq.empty)
  }

}
