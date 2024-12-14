package com.github.cosminci.aoc._2024

import com.github.cosminci.aoc.utils
import monocle.syntax.all._

object Day14 {

  def main(args: Array[String]): Unit = {
    val robots = parseInput(utils.loadInputAsStrings("2024/day14.txt"))

    println(s"Part 1: ${safetyFactor(robots)}")
    println(s"Part 2: ${stepsUntilChristmasTree(robots)}")
  }

  private val (gridWidth, gridHeight)  = (101, 103)

  final case class Pos(x: Int, y: Int)
  final case class Robot(pos: Pos, dx: Int, dy: Int)

  def safetyFactor(robots: Seq[Robot]): Int =
    Iterator.iterate(robots)(_.map(moveRobot)).drop(100).next()
      .filter(r => r.pos.x != gridWidth / 2 && r.pos.y != gridHeight / 2)
      .groupBy(r => (r.pos.x / (gridWidth / 2 + 1), r.pos.y / (gridHeight / 2 + 1)))
      .map { case (_, quadrantRobots) => quadrantRobots.size }
      .product

  def stepsUntilChristmasTree(robots: Seq[Robot]): Int =
    Iterator.iterate((robots, 0)) { case (robots, steps) => (robots.map(moveRobot), steps + 1) }
      .collectFirst { case (robots, steps) if containsTreeTriangle(robots) => steps }.get

  private val triangleFromTip = (1 to 5).flatMap(i => Seq((-i, i), (i, i)))
  private def containsTreeTriangle(robots: Seq[Robot]) = {
    val positions = robots.map(_.pos).toSet
    robots.exists { case Robot(Pos(tipX, tipY), _, _) =>
      triangleFromTip.forall { case (dx, dy) => positions.contains(Pos(tipX + dx, tipY + dy)) }
    }
  }

  private def moveRobot(r: Robot) =
    r.focus(_.pos).modify(p => Pos((p.x + r.dx + gridWidth) % gridWidth, (p.y + r.dy + gridHeight) % gridHeight))

  private def parseInput(lines: Seq[String]) =
    lines.map { case s"p=$x,$y v=$dx,$dy" =>
      Robot(Pos(x.toInt, y.toInt), dx.toInt, dy.toInt)
    }

}
