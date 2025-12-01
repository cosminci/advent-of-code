package com.github.cosminci.aoc._2025

import com.github.cosminci.aoc.utils.loadInputAsStrings

object Day1 {

  private val startPos = 50
  private val dialSize = 100

  def main(args: Array[String]): Unit = {
    val commands = parseCommands(loadInputAsStrings("2025/day1.txt"))

    println(s"Part 1: ${part1(commands)}")
    println(s"Part 2: ${part2(commands)}")
  }

  def part1(commands: Seq[Int]): Int =
    commands
      .scanLeft(startPos)((pos, move) => (pos + dialSize + move) % dialSize)
      .count(_ == 0)

  def part2(rotations: Seq[Int]): Int =
    rotations
      .scanLeft(Seq(startPos))(findAllPositions)
      .flatten
      .count(_ == 0)

  private def findAllPositions(pos: Seq[Int], rotation: Int) = {
    val step = if (rotation < 0) -1 else 1
    (1 to rotation.abs).scanLeft(pos.last)((pos, _) => (pos + dialSize + step) % dialSize).tail
  }

  private def parseCommands(input: Seq[String]) =
    input.map(r => if (r.head == 'L') -r.tail.toInt else r.tail.toInt)

}
