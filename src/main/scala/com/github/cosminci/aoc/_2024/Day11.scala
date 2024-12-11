package com.github.cosminci.aoc._2024

import com.github.cosminci.aoc.utils

import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps

object Day11 {

  def main(args: Array[String]): Unit = {
    val stones = utils.loadInputAsStrings("2024/day11.txt").head.split(' ')

    println(s"Part 1: ${stoneCountAfterBlinks(stones, count = 25)}")
    println(s"Part 2: ${stoneCountAfterBlinks(stones, count = 75)}")
  }

  def stoneCountAfterBlinks(stones: Array[String], count: Int): Long = {
    val mem = mutable.Map.empty[(String, Int), Long]
    def dfs(stone: String, count: Int): Long =
      mem.getOrElseUpdate((stone, count), {
        if (count == 0) 1L
        else blink(stone).map(dfs(_, count - 1)).sum
      })
    stones.map(dfs(_, count)).sum
  }

  private def blink(stone: String) =
    if (stone == "0") Vector("1")
    else if (stone.length % 2 == 1) Vector((stone.toLong * 2024).toString)
    else stone.splitAt(stone.length / 2).pipe { case (a, b) => Vector(a, b.toLong.toString) }

}
