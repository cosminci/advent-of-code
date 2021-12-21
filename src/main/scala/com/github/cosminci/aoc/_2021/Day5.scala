package com.github.cosminci.aoc._2021

import com.github.cosminci.aoc.utils

object Day5 {
  def main(args: Array[String]): Unit = {
    val input = utils.loadInputAsStrings("2021/day5.txt")
    val lines = input.map(s => s.split(" -> ").flatMap(_.split(',').map(_.toInt)))

    println(s"Part I: ${countOverlappingPoints(lines.filter { case Array(x1, y1, x2, y2) => x1 == x2 || y1 == y2 })}")
    println(s"Part II: ${countOverlappingPoints(lines)}")
  }

  def countOverlappingPoints(lines: Seq[Array[Int]]): Int =
    lines.foldLeft(Map.empty[(Int, Int), Int].withDefaultValue(0)) {
      case (counter, Array(x1, y1, x2, y2)) =>
        val x = x1 to x2 by Option.when(x1 < x2)(1).getOrElse(-1)
        val y = y1 to y2 by Option.when(y1 < y2)(1).getOrElse(-1)
        x.zipAll(y, x1, y1).foldLeft(counter)((counter, point) => counter.updated(point, counter(point) + 1))
    }.count { case (_, count) => count >= 2 }
}
