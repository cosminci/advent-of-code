package com.github.cosminci.aoc._2017

import com.github.cosminci.aoc.utils

object Day11 {

  def main(args: Array[String]): Unit = {
    val steps = utils.loadInputAsStrings("2017/day11.txt").head.split(',')

    println(s"Part 1: ${finalDistance(steps)}")
    println(s"Part 2: ${farthestDistance(steps)}")
  }

  def finalDistance(steps: Array[String]): Int =
    (distanceFromOrigin _).tupled(pathTaken(steps).last)

  def farthestDistance(steps: Array[String]): Int =
    pathTaken(steps).map((distanceFromOrigin _).tupled).max

  private def distanceFromOrigin(x: Int, y: Int) =
    x.abs.min(y.abs) + x.abs.max(y.abs) - x.abs.min(y.abs)

  private def pathTaken(steps: Array[String]) =
    steps.scanLeft((0, 0)) { case ((x, y), dir) =>
      dir match {
        case "n"  => (x, y + 1)
        case "ne" => (x + 1, y + 1)
        case "se" => (x + 1, y)
        case "s"  => (x, y - 1)
        case "sw" => (x - 1, y - 1)
        case "nw" => (x - 1, y)
      }
    }

}
