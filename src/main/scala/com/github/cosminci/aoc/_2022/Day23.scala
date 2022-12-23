package com.github.cosminci.aoc._2022

import com.github.cosminci.aoc.utils

import scala.util.chaining.scalaUtilChainingOps

object Day23 {

  def main(args: Array[String]): Unit = {
    val input = utils.loadInputAsStrings("2022/day23.txt")
    val elves = parseInput(input).toSet

    println(s"Part 1: ${emptyGroundAfterTenRounds(elves)}")
    println(s"Part 2: ${countRoundsUntilNoMoves(elves)}")
  }

  def emptyGroundAfterTenRounds(elves: Set[Pos]): Int = {
    val finalPositions  = (0 until 10).foldLeft(elves)(move)
    val Pos(xMin, yMin) = finalPositions.reduce(_ min _)
    val Pos(xMax, yMax) = finalPositions.reduce(_ max _)
    (xMax - xMin + 1) * (yMax - yMin + 1) - elves.size
  }

  def countRoundsUntilNoMoves(elves: Set[Pos]): Int =
    Iterator
      .iterate((elves, 0)) { case (elves, round) => (move(elves, round), round + 1) }
      .sliding(2)
      .dropWhile { case Seq((curr, _), (next, _)) => curr != next }.next()
      .pipe { case Seq((_, round), _) => round + 1 }

  case class Pos(x: Int, y: Int) {
    def min(other: Pos): Pos = Pos(x min other.x, y min other.y)
    def max(other: Pos): Pos = Pos(x max other.x, y max other.y)
    def +(other: Pos): Pos   = Pos(x + other.x, y + other.y)
  }

  private val directions = Seq(
    Seq(Pos(-1, -1), Pos(-1, 0), Pos(-1, 1)),
    Seq(Pos(1, -1), Pos(1, 0), Pos(1, 1)),
    Seq(Pos(-1, -1), Pos(0, -1), Pos(1, -1)),
    Seq(Pos(-1, 1), Pos(0, 1), Pos(1, 1))
  )
  private val allDirections = directions.flatten.toSet

  private def move(elves: Set[Pos], round: Int): Set[Pos] = {
    val (moving, sitting) = elves.partition(pos => allDirections.map(_ + pos).intersect(elves).nonEmpty)
    sitting ++ moving
      .map(pos => pos -> propose(elves, round, pos))
      .groupMap { case (_, next) => next } { case (curr, _) => curr }.toSeq
      .flatMap { case (next, curr) => if (curr.size == 1) Seq(next) else curr }.toSet
  }

  private def propose(elves: Set[Pos], round: Int, pos: Pos) =
    (round until round + directions.length)
      .map(i => directions(i % directions.length).map(_ + pos))
      .collectFirst { case dir if (dir.toSet intersect elves).isEmpty => dir(1) }
      .getOrElse(pos)

  private def parseInput(input: Seq[String]) =
    for {
      row <- input.indices
      col <- input(row).indices
      if input(row)(col) == '#'
    } yield Pos(row, col)

}
