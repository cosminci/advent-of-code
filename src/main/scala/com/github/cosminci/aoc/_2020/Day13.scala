package com.github.cosminci.aoc._2020

import com.github.cosminci.aoc.utils

import scala.annotation.tailrec

object Day13 {

  def main(args: Array[String]): Unit = {
    val input          = utils.loadInputAsStrings("2020/day13.txt")
    val (minTs, buses) = (input.head.toInt, input.last.split(','))

    println(s"Part 1: ${earliestBusIdTimesWaitTime(minTs, buses)}")
    println(s"Part 2: ${earliestTsBusesDepartSubsequently(buses)}")
  }

  def earliestBusIdTimesWaitTime(minTs: Int, buses: Seq[String]): Int =
    buses
      .filter(_ != "x")
      .map(_.toInt)
      .map { busId =>
        val departureDelta = (minTs / busId + 1) * busId - minTs
        Seq(busId, departureDelta)
      }
      .minBy(_.last)
      .product

  def earliestTsBusesDepartSubsequently(buses: Seq[String]): Long = {
    val numsAndRemainders = buses.zipWithIndex.collect {
      case (bus, offset) if bus != "x" =>
        val busId = bus.toLong
        (busId, (busId - offset) % busId)
    }
    crt(numsAndRemainders)
  }

  /** Chinese Remainder Theorem * */
  def crt(moduliAndRemainders: Seq[(Long, Long)]): Long = {
    val product = moduliAndRemainders.map(_._1).product
    moduliAndRemainders.foldLeft(0L) { case (acc, (number, remainder)) =>
      val q = product / number
      acc + remainder * mulInv(q, number) * q
    } % product
  }

  def mulInv(a: Long, b: Long): Long = {
    @tailrec
    def loop(a: Long, b: Long, x0: Long, x1: Long): Long =
      if (a > 1) loop(b, a % b, x1 - (a / b) * x0, x0) else x1

    if (b == 1) 1
    else {
      val x1 = loop(a, b, 0, 1)
      if (x1 < 0) x1 + b else x1
    }
  }
}
