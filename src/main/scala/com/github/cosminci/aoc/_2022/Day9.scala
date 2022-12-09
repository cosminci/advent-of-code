package com.github.cosminci.aoc._2022

import com.github.cosminci.aoc.utils

object Day9 {

  def main(args: Array[String]): Unit = {
    val input = utils.loadInputAsStrings("2022/day9.txt")
    val moves = parseInput(input)

    println(s"Part 1: ${countTailVisitedPositions(moves, ropeLength = 2)}")
    println(s"Part 2: ${countTailVisitedPositions(moves, ropeLength = 10)}")
  }

  private val delta = Map('L' -> (0, -1), 'R' -> (0, 1), 'D' -> (-1, 0), 'U' -> (1, 0))

  def countTailVisitedPositions(moves: Seq[Char], ropeLength: Int): Int =
    moves
      .map(delta)
      .scanLeft(Seq.fill(ropeLength)((0, 0)))(moveRope)
      .map(_.last)
      .distinct
      .length

  private def moveRope(rope: Seq[(Int, Int)], delta: (Int, Int)) = {
    val (dx, dy) = delta

    val (headX, headY) +: tail = rope
    val (newHeadX, newHeadY)   = (headX + dx, headY + dy)

    (newHeadX, newHeadY) +: moveTail((newHeadX, newHeadY), tail)
  }

  private def moveTail(prevKnot: (Int, Int), remainingKnots: Seq[(Int, Int)]): Seq[(Int, Int)] =
    remainingKnots match {
      case Nil => Seq.empty
      case currKnot +: tail =>
        val movedKnot = moveKnot(prevKnot, currKnot)
        movedKnot +: moveTail(movedKnot, tail)
    }

  private def moveKnot(prevKnot: (Int, Int), currKnot: (Int, Int)) = (prevKnot, currKnot) match {
    case ((endX, endY), (startX, startY)) =>
      val (dx, dy) = (endX - startX, endY - startY)

      if (dx.abs <= 1 && dy.abs <= 1) currKnot
      else (startX + dx.sign, startY + dy.sign)
  }

  private def parseInput(input: Seq[String]) = {
    val pattern = "([LRDU]) ([0-9]+)".r
    input.flatMap { case pattern(direction, numSteps) =>
      Seq.fill(numSteps.toInt)(direction.head)
    }
  }
}
