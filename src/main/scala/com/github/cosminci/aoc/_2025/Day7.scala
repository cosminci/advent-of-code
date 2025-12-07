package com.github.cosminci.aoc._2025

import com.github.cosminci.aoc.utils.loadInputAsStrings

import scala.collection.mutable

object Day7 {

  def main(args: Array[String]): Unit = {
    val (start, grid) = parseInput(loadInputAsStrings("2025/day7.txt"))

    println(s"Part 1: ${beamSplitCount(start, grid)}")
    println(s"Part 2: ${timelineCount(start, grid)}")
  }

  final case class Pos(r: Int, c: Int)

  def beamSplitCount(start: Pos, grid: Seq[String]): Int = {
    @annotation.tailrec
    def dfs(beams: Seq[Pos], count: Int): Int =
      if (beams.exists(_.r == grid.length - 1)) count
      else {
        val newBeams = beams.flatMap(moveBeam(_, grid))
        dfs(newBeams.distinct, count + newBeams.size - beams.size)
      }
    dfs(Seq(start), count = 0)
  }

  def timelineCount(start: Pos, grid: Seq[String]): Long = {
    val mem = mutable.Map.empty[Pos, Long]
    def dfs(beam: Pos): Long = mem.getOrElseUpdate(beam,
      if (beam.r == grid.length - 1) 1
      else moveBeam(beam, grid).map(dfs).sum
    )
    dfs(start)
  }

  private def moveBeam(beam: Pos, grid: Seq[String]) =
    if (grid(beam.r + 1)(beam.c) == '.') Seq(Pos(beam.r + 1, beam.c))
    else Seq(Pos(beam.r + 1, beam.c - 1), Pos(beam.r + 1, beam.c + 1))

  private def parseInput(lines: Seq[String]) =
    lines.indices
      .flatMap(r => lines(r).indices.map(c => (r, c)))
      .collectFirst { case (r, c) if lines(r)(c) == 'S' => Pos(r, c) }
      .map(start => (start, lines)).get

}
