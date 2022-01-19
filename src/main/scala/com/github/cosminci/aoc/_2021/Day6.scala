package com.github.cosminci.aoc._2021

import com.github.cosminci.aoc.utils

import scala.collection.mutable

object Day6 {
  def main(args: Array[String]): Unit = {
    val fish = utils.loadInputAsStrings("2021/day6.txt").head.split(',').map(_.toInt).toSeq
    println(s"Part I: ${simulateTopDownDP(fish, 80)}")
    println(s"Part II: ${simulateTopDownDP(fish, 256)}")

    println(s"Part I: ${simulateSlidingTimeline(fish, 80)}")
    println(s"Part II: ${simulateSlidingTimeline(fish, 256)}")
  }

  def simulateTopDownDP(fish: Seq[Int], daysLeft: Int): Long = {
    val mem = mutable.Map.empty[(Int, Int), Long]
    def dfs(delay: Int, daysLeft: Int): Long =
      mem.getOrElseUpdate((delay, daysLeft), {
        if (daysLeft == 0) 1
        else if (delay > 0) dfs(delay - 1, daysLeft - 1)
        else dfs(6, daysLeft - 1) + dfs(8, daysLeft - 1)
      })
    fish.map(dfs(_, daysLeft)).sum
  }

  def simulateSlidingTimeline(fish: Seq[Int], daysLeft: Int): Long = {
    val initialSchedule = fish.foldLeft(Seq.fill(9)(0L)) { (schedule, fish) =>
      schedule.updated(fish, schedule(fish) + 1)
    }
    (1 to daysLeft).foldLeft(initialSchedule) { (schedule, _) =>
      schedule.updated(7, schedule(7) + schedule.head).tail :+ schedule.head
    }.sum
  }
}
