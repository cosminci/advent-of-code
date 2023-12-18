package com.github.cosminci.aoc._2023

import com.github.cosminci.aoc.utils

object Day18 {

  def main(args: Array[String]): Unit = {
    val input = utils.loadInputAsStrings("2023/day18.txt")

    val instructions = parseInput(input)

    println(s"Part 1: ${lavaCapacityGivenLines(instructions.map(_.line))}")
    println(s"Part 2: ${lavaCapacityGivenHexCodes(instructions.map(_.hexCode))}")
  }

  final case class Line(dir: Char, len: Int)
  final case class Input(line: Line, hexCode: String)
  final case class Pos(r: Long, c: Long)

  def lavaCapacityGivenLines(lines: Seq[Line]): Long =
    polygonArea(lines)

  def lavaCapacityGivenHexCodes(input: Seq[String]): Long =
    polygonArea(input.map(parseLineFromHex))

  private def polygonArea(lines: Seq[Line]) = {
    val boundary = lines.map(_.len.toLong).sum
    val vertices = buildPolygon(lines)
    val interior = vertices.sliding(2).toSeq.map { case Seq(p1, p2) => p1.r * p2.c - p1.c * p2.r }.sum / 2

    interior.abs + boundary / 2 + 1
  }

  private val directions = Map('R' -> Pos(0, 1), 'L' -> Pos(0, -1), 'U' -> Pos(-1, 0), 'D' -> Pos(1, 0))
  private def buildPolygon(instructions: Seq[Line]) =
    instructions.scanLeft(Pos(0, 0)) { case (prev, Line(dir, len)) =>
      Pos(prev.r + directions(dir).r * len, prev.c + directions(dir).c * len)
    }

  private val directionEncoding = Map(0 -> 'R', 1 -> 'D', 2 -> 'L', 3 -> 'U')
  private def parseLineFromHex(hexCode: String) =
    Line(directionEncoding(hexCode.last - '0'), Integer.parseInt(hexCode.take(5), 16))

  private def parseInput(input: Seq[String]) =
    input.map { case s"$dir $len (#$hexCode)" => Input(Line(dir.head, len.toInt), hexCode) }

}
