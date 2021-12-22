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
  case class Cuboid(x: Range, y: Range, z: Range)

  def processCommands(commands: Seq[Command]): Seq[Cuboid] =
    commands.foldLeft(Seq.empty[Cuboid]) { case (prevCuboids, Command(turnOn, cuboid)) =>
      val cuboids = prevCuboids.flatMap(removeOverlap(_, cuboid))
      cuboids ++ Option.when(turnOn)(cuboid)
    }

  private def overlap(r1: Range, r2: Range) = r1.low <= r2.high && r2.low <= r1.high

  private def splitRange(r1: Range, r2: Range): Seq[Range] =
    if (r1.low < r2.low) Range(r1.low, r2.low - 1) +: splitRange(Range(r2.low, r1.high), r2)
    else if (r1.high > r2.high) Range(r2.high + 1, r1.high) +: splitRange(Range(r1.low, r2.high), r2)
    else Seq(r1)

  private def removeOverlap(c1: Cuboid, c2: Cuboid): Seq[Cuboid] =
    if (!(overlap(c1.x, c2.x) && overlap(c1.y, c2.y) && overlap(c1.z, c2.z)))
      Seq(c1)
    else for {
      xr <- splitRange(c1.x, c2.x)
      yr <- splitRange(c1.y, c2.y)
      zr <- splitRange(c1.z, c2.z)
      if !(overlap(xr, c2.x) && overlap(yr, c2.y) && overlap(zr, c2.z))
    } yield Cuboid(xr, yr, zr)

  private def length(r: Range): Long  = r.high - r.low + 1
  private def volume(c: Cuboid): Long = Seq(c.x, c.y, c.z).map(length).product

  private def isInitialization(c: Command): Boolean = {
    val ranges = Seq(c.cuboid.x, c.cuboid.y, c.cuboid.z)
    (ranges.map(_.low) ++ ranges.map(_.high)).forall(v => v >= -50 && v <= 50)
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
