package com.github.cosminci.aoc._2021

import com.github.cosminci.aoc.utils

import scala.collection.mutable

object Day6 {
  def main(args: Array[String]): Unit = {
    val fish = utils.loadInputAsStrings("2021/day6.txt").head.split(',').map(_.toInt)
    println(s"Part I: ${simulate(fish, 80)}")
    println(s"Part II: ${simulate(fish, 256)}")
  }

  def simulate(fish: Seq[Int], daysLeft: Int): Long = {
    val mem = mutable.Map.empty[(Int, Int), Long]
    def dfs(delay: Int, daysLeft: Int): Long =
      mem.getOrElseUpdate((delay, daysLeft), {
        if (daysLeft == 0) 1
        else if (delay > 0) dfs(delay - 1, daysLeft - 1)
        else dfs(6, daysLeft - 1) + dfs(8, daysLeft - 1)
      })
    fish.map(dfs(_, daysLeft)).sum
  }
}
