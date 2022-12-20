package com.github.cosminci.aoc._2022

import com.github.cosminci.aoc.utils

import scala.collection.mutable

object Day20 {

  def main(args: Array[String]): Unit = {
    val input = utils.loadInputAsLongs("2022/day20.txt")

    println(s"Part 1: ${groveCoordinateSum(nums = input, mixCount = 1)}")
    println(s"Part 2: ${groveCoordinateSum(nums = input.map(_ * 811589153), mixCount = 10)}")
  }

  def groveCoordinateSum(nums: Seq[Long], mixCount: Int): Long = {
    val indexedList = mutable.IndexedBuffer.from(nums.zipWithIndex)
    (1 to mixCount).foreach { _ =>
      nums.zipWithIndex.foreach { case num @ (value, _) =>
        val prevIdx = indexedList.indexOf(num)
        val newIdx  = math.floorMod(prevIdx + value, nums.length - 1).toInt
        indexedList.remove(prevIdx)
        indexedList.insert(newIdx, num)
      }
    }
    val zeroIdx = indexedList.indexWhere { case (value, _) => value == 0 }
    Seq(1000, 2000, 3000)
      .map(idx => indexedList((idx + zeroIdx) % nums.length))
      .map { case (value, _) => value }
      .sum
  }

}
