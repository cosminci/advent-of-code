package com.github.cosminci.aoc._2022

import com.github.cosminci.aoc.utils

object Day15 {

  def main(args: Array[String]): Unit = {
    val input    = utils.loadInputAsStrings("2022/day15.txt")
    val readings = parseInput(input)

    println(s"Part 1: ${countInvalidPositions(readings, y = 2_000_000)}")
    println(s"Part 2: ${findTuningFrequency(readings, maxY = 4_000_000)}")
  }

  case class Pos(x: Int, y: Int)

  def countInvalidPositions(readings: Seq[(Pos, Pos)], y: Int): Int = {
    val beaconCount = readings.collect { case (_, beacon) if beacon.y == y => beacon.x }.distinct.size
    scanLine(readings, y).map(range => range.last - range.start + 1 - beaconCount).sum
  }

  def findTuningFrequency(readings: Seq[(Pos, Pos)], maxY: Int): Option[Long] =
    (0 to maxY).iterator
      .map(y => (y, scanLine(readings, y)))
      .collectFirst { case (y, Seq(r1, _)) => (r1.last + 1).toLong * 4_000_000 + y }

  private def scanLine(readings: Seq[(Pos, Pos)], y: Int): Seq[Range] =
    readings
      .flatMap { case (sensor, beacon) =>
        val totalRange     = (sensor.x - beacon.x).abs + (sensor.y - beacon.y).abs
        val distanceToLine = (sensor.y - y).abs
        val rangeRemaining = totalRange - distanceToLine
        Option.when(rangeRemaining >= 0)(sensor.x - rangeRemaining to sensor.x + rangeRemaining)
      }
      .sortBy(_.start)
      .foldLeft(Seq.empty[Range])(mergeIntervals)

  private def mergeIntervals(prev: Seq[Range], curr: Range) =
    prev match {
      case Nil => prev :+ curr
      case head :+ tail =>
        if (tail.last < curr.start) prev :+ curr
        else head :+ (tail.start to curr.last.max(tail.last))
    }

  private def parseInput(input: Seq[String]) = {
    val pattern = "Sensor at x=(-?[0-9]+), y=(-?[0-9]+): closest beacon is at x=(-?[0-9]+), y=(-?[0-9]+)".r
    input.map { case pattern(sx, sy, bx, by) => (Pos(sx.toInt, sy.toInt), Pos(bx.toInt, by.toInt)) }
  }

}
