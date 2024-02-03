package com.github.cosminci.aoc._2018

import com.github.cosminci.aoc.utils

import scala.util.chaining._

object Day10 {

  def main(args: Array[String]): Unit = {
    val lights = parseInput(utils.loadInputAsStrings("2018/day10.txt"))

    println(s"Part 1: ").tap(_ => drawAlignedLights(lights))
    println(s"Part 2: ${timeToAlignLights(lights)}")
  }

  final case class Light(x: Int, y: Int, dx: Int, dy: Int)

  def drawAlignedLights(lights: Seq[Light]): Unit =
    alignLights(lights).pipe { case (_, lights) => drawResult(lights.toSet) }

  def timeToAlignLights(lights: Seq[Light]): Int =
    alignLights(lights).pipe { case (time, _) => time }

  private def alignLights(lights: Seq[Light]): (Int, Seq[Light]) = {
    @annotation.tailrec
    def dfs(time: Int, lights: Seq[Light]): (Int, Seq[Light]) =
      if (lights.map(_.y).distinct.size <= 10) (time, lights)
      else dfs(time + 1, lights.map { case Light(x, y, dx, dy) => Light(x + dx, y + dy, dx, dy) })

    dfs(time = 0, lights)
  }

  private def drawResult(lights: Set[Light]): Unit = {
    val (minX, maxX) = lights.map(_.x).min -> lights.map(_.x).max
    val (minY, maxY) = lights.map(_.y).min -> lights.map(_.y).max

    (minY to maxY).foreach { y =>
      (minX to maxX).foreach { x =>
        if (lights.exists(l => l.x == x && l.y == y)) print("#") else print(".")
      }
      println()
    }
  }

  private def parseInput(input: Seq[String]) =
    input.map { case s"position=<$x, $y> velocity=<$dx, $dy>" =>
      Light(x.trim.toInt, y.trim.toInt, dx.trim.toInt, dy.trim.toInt)
    }

}
