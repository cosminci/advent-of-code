package com.github.cosminci.aoc._2023

import com.github.cosminci.aoc.utils

object Day6 {

  def main(args: Array[String]): Unit = {
    val input = utils.loadInputAsStrings("2023/day6.txt")

    println(s"Part 1: ${waysToWinProduct(input)}")
    println(s"Part 2: ${waysToWinSingle(input)}")
  }

  def waysToWinProduct(input: Seq[String]): Long = {
    val times     = input.head.replace("Time:", "").split(' ').filterNot(_.isEmpty).map(_.toLong)
    val distances = input.last.replace("Distance:", "").split(' ').filterNot(_.isEmpty).map(_.toLong)

    times.zip(distances).map((waysToWinRace _).tupled).product
  }

  def waysToWinSingle(input: Seq[String]): Long = {
    val time     = input.head.replace("Time:", "").replace(" ", "").toLong
    val distance = input.last.replace("Distance:", "").replace(" ", "").toLong

    waysToWinRace(time, distance)
  }

  private def waysToWinRace(time: Long, distance: Long): Long = {
    def distanceAfter(waitTime: Long) = waitTime * (time - waitTime)

    val minWait = binarySearch(l = 0, r = time, stopFn = distanceAfter(_) > distance)
    val maxWait = binarySearch(l = minWait, r = time, stopFn = distanceAfter(_) <= distance)

    maxWait - minWait
  }

  @annotation.tailrec
  private def binarySearch(l: Long, r: Long, stopFn: Long => Boolean): Long =
    if (l >= r) l
    else {
      val mid = l + (r - l) / 2
      if (stopFn(mid)) binarySearch(l, mid, stopFn)
      else binarySearch(mid + 1, r, stopFn)
    }

}
