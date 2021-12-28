package com.github.cosminci.aoc._2020

import com.github.cosminci.aoc.utils

object Day5 {

  def main(args: Array[String]): Unit = {
    val input = utils.loadInputAsStrings("2020/day5.txt").map(parseInput)

    println(s"Part 1: ${highestSeatId(input)}")
    println(s"Part 2: ${findEmptySeat(input)}")
  }

  def highestSeatId(input: Seq[SeatLocator]): Int =
    input.map(calculateSeatId).max

  def findEmptySeat(input: Seq[SeatLocator]): Option[Int] = {
    val seats = input.map(calculateSeatId).toSet
    (seats.min + 1 until seats.max).find { id =>
      !seats.contains(id) && seats.contains(id - 1) && seats.contains(id + 1)
    }
  }

  private def parseInput(input: String) = {
    val (rowLocator, columnLocator) = input.toCharArray.splitAt(7)
    SeatLocator(rowLocator, columnLocator)
  }

  private def calculateSeatId(seatLocator: SeatLocator): Int =
    locatePosition(seatLocator.row, Range(0, 127), 'F', 'B') * 8 +
      locatePosition(seatLocator.column, Range(0, 7), 'L', 'R')

  private def locatePosition(locator: Array[Char], maxRange: Range, moveDownSentinel: Char, moveUpSentinel: Char) =
    locator.foldLeft(maxRange) { (range, direction) =>
      direction match {
        case `moveDownSentinel` => Range(range.low, (range.high + range.low) / 2)
        case `moveUpSentinel` => Range((range.high + range.low + 1) / 2, range.high)
      }
    }.low

  case class SeatLocator(row: Array[Char], column: Array[Char])
  case class Range(low: Int, high: Int)
}
