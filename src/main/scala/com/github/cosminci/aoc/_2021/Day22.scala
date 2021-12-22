package com.github.cosminci.aoc._2021

import com.github.cosminci.aoc.utils

object Day22 {
  def main(args: Array[String]): Unit = {
    val commands = utils.loadInputAsStrings("2021/day22.txt").map(parseLine)

    println(s"Part I: ${processCommands(commands.filter(isInitialization)).map(volume).sum}")
    println(s"Part II: ${processCommands(commands).map(volume).sum}")
  }

  case class Range(low: Int, high: Int)
  case class Command(turnOn: Boolean, cuboid: Cuboid)
  case class Cuboid(xRange: Range, yRange: Range, zRange: Range)

  def processCommands(commands: Seq[Command]): Seq[Cuboid] =
    commands.foldLeft(Seq.empty[Cuboid]) { case (prevCuboids, Command(turnOn, cuboid)) =>
      val cuboids = prevCuboids.flatMap(removeOverlap(_, cuboid))
      cuboids ++ Option.when(turnOn)(cuboid)
    }

  private val disjoint = (r1: Range, r2: Range) => r1.low > r2.high || r2.low > r1.high
  private def disjoint(c1: Cuboid, c2: Cuboid): Boolean =
    Seq((c1.xRange, c2.xRange), (c1.yRange, c2.yRange), (c1.zRange, c2.zRange)).exists(disjoint.tupled)

  private def splitRange(r1: Range, r2: Range): Seq[Range] =
    if (r1.low < r2.low) Range(r1.low, r2.low - 1) +: splitRange(Range(r2.low, r1.high), r2)
    else if (r1.high > r2.high) Range(r2.high + 1, r1.high) +: splitRange(Range(r1.low, r2.high), r2)
    else Seq(r1)

  private def removeOverlap(c1: Cuboid, c2: Cuboid): Seq[Cuboid] =
    if (disjoint(c1, c2)) Seq(c1)
    else for {
      xRange <- splitRange(c1.xRange, c2.xRange)
      yRange <- splitRange(c1.yRange, c2.yRange)
      zRange <- splitRange(c1.zRange, c2.zRange)
      subCuboid = Cuboid(xRange, yRange, zRange) if disjoint(subCuboid, c2)
    } yield subCuboid

  private def length(r: Range): Long  = r.high - r.low + 1
  private def volume(c: Cuboid): Long = Seq(c.xRange, c.yRange, c.zRange).map(length).product

  private def isInitialization(c: Command): Boolean = {
    val ranges = Seq(c.cuboid.xRange, c.cuboid.yRange, c.cuboid.zRange)
    (ranges.map(_.low) ++ ranges.map(_.high)).forall(pos => pos >= -50 && pos <= 50)
  }

  private def parseLine(s: String): Command = {
    val Array(state, cuboid) = s.split(" ")
    val Array(x, y, z) = cuboid
      .split(',')
      .map(_.split("\\.\\."))
      .map { case Array(low, high) => Range(low.drop(2).toInt, high.toInt) }
    Command(turnOn = state == "on", Cuboid(x, y, z))
  }
}
