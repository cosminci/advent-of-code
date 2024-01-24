package com.github.cosminci.aoc._2017

import com.github.cosminci.aoc.utils

object Day13 {

  def main(args: Array[String]): Unit = {
    val scanners = parseInput(utils.loadInputAsStrings("2017/day13.txt"))

    println(s"Part 1: ${caughtSeverity(scanners)}")
    println(s"Part 2: ${minTimeToStartWithoutGettingCaught(scanners)}")
  }

  def caughtSeverity(scanners: Map[Int, Int]): Int =
    (0 to scanners.keys.max).flatMap { depth =>
      scanners.get(depth).map(range => if (scannerPositionAtTime(range, depth) == 0) depth * range else 0)
    }.sum

  def minTimeToStartWithoutGettingCaught(scanners: Map[Int, Int]): Option[Int] =
    Iterator.iterate(0)(_ + 1).find { delay: Int =>
      scanners.forall { case (depth, range) => scannerPositionAtTime(range, depth + delay) != 0 }
    }

  private def scannerPositionAtTime(range: Int, time: Int) =
    time % (2 * (range - 1)) match {
      case i if i < range - 1 => i
      case i                  => 2 * (range - 1) - i
    }

  private def parseInput(lines: Seq[String]) =
    lines.map { case s"$depth: $range" => depth.toInt -> range.toInt }.toMap

}
