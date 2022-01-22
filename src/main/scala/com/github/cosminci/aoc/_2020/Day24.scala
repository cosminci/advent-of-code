package com.github.cosminci.aoc._2020

import com.github.cosminci.aoc.utils

object Day24 {

  def main(args: Array[String]): Unit = {
    val input      = utils.loadInputAsStrings("2020/day24.txt")
    val directions = parseInput(input)

    println(s"Part 1: ${countBlackTiles(directions)}")
    println(s"Part 2: ${countBlackTilesAfterDays(directions, 100)}")
  }

  def countBlackTiles(allDirections: Seq[Seq[String]]): Int =
    findBlackTiles(allDirections).size

  def countBlackTilesAfterDays(allDirections: Seq[Seq[String]], days: Int): Int =
    Iterator
      .iterate(findBlackTiles(allDirections))(tiles => expand(tiles).filter(isBlackAfterDay(tiles, _)))
      .drop(days)
      .next()
      .size

  private def isBlackAfterDay(blackTiles: Set[(Int, Int)], tile: (Int, Int)) = {
    val adjacentBlack = countAdjacentBlack(blackTiles, tile)
    (blackTiles.contains(tile) && Seq(1, 2).contains(adjacentBlack)) || adjacentBlack == 2
  }

  private def countAdjacentBlack(blackTiles: Set[(Int, Int)], tile: (Int, Int)) =
    steps.values.count { case (dx, dy) => blackTiles.contains((tile._1 + dx, tile._2 + dy)) }

  private def expand(blackTiles: Set[(Int, Int)]) =
    blackTiles.flatMap { case t @ (x, y) =>
      steps.values.map { case (dx, dy) => (x + dx, y + dy) }.toSet + t
    }

  private def findBlackTiles(allDirections: Seq[Seq[String]]) =
    allDirections
      .foldLeft(Map.empty[(Int, Int), Int].withDefaultValue(0)) { case (tiles, directions) =>
        val tile = directions.map(steps).foldLeft(0, 0) { case ((x, y), (dx, dy)) => (x + dx, y + dy) }
        tiles.updated(tile, tiles(tile) + 1)
      }
      .filter { case (_, flipCount) => flipCount % 2 == 1 }
      .keySet

  private val steps = Map(
    "E"  -> (1, 0),
    "SE" -> (1, -1),
    "NE" -> (0, 1),
    "W"  -> (-1, 0),
    "SW" -> (0, -1),
    "NW" -> (-1, 1)
  )

  private def parseInput(input: Seq[String]) =
    input.map(s => parseLine(s.toCharArray.toSeq))

  private def parseLine(s: Seq[Char]): Seq[String] =
    s match {
      case Seq()       => Seq.empty
      case 'e' +: tail => "E" +: parseLine(tail)
      case 'w' +: tail => "W" +: parseLine(tail)
      case 's' +: tail =>
        Option.when(tail.head == 'e')("SE").getOrElse("SW") +: parseLine(tail.tail)
      case 'n' +: tail =>
        Option.when(tail.head == 'e')("NE").getOrElse("NW") +: parseLine(tail.tail)
    }
}
