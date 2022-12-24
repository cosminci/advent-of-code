package com.github.cosminci.aoc._2022

import com.github.cosminci.aoc.utils

import scala.util.chaining._

object Day24 {

  def main(args: Array[String]): Unit = {
    val input = utils.loadInputAsStrings("2022/day24.txt")

    val grid         = input.tail.dropRight(1).map(_.tail.dropRight(1))
    val (m, n)       = (grid.length - 1, grid.head.length - 1)
    val (start, end) = ((-1, 0), (m + 1, n))
    val blizzard     = mapBlizzard(m, n)(grid).toMap

    println(s"Part 1: ${fewestMinutesFromStartToEnd(m, n)(blizzard, start, end).pipe { case (minutes, _) => minutes }}")
    println(s"Part 2: ${fewestMinutesToRunElfErrands(m, n)(blizzard, start, end)}")
  }

  type Pos      = (Int, Int)
  type Dir      = (Int, Int)
  type Blizzard = Map[Pos, Seq[Dir]]

  def fewestMinutesFromStartToEnd(m: Int, n: Int)(blizzard: Blizzard, start: Pos, end: Pos): (Int, Blizzard) =
    Iterator
      .iterate((Set(start), blizzard, 0))((forwardMinute(m, n) _).tupled)
      .dropWhile { case (positions, _, _) => !positions.contains(end) }
      .next()
      .pipe { case (_, blizzard, minutes) => (minutes, blizzard) }

  def fewestMinutesToRunElfErrands(m: Int, n: Int)(blizzard: Blizzard, start: Pos, end: Pos): Int = {
    val (steps1, blizzard1) = fewestMinutesFromStartToEnd(m, n)(blizzard, start, end)
    val (steps2, blizzard2) = fewestMinutesFromStartToEnd(m, n)(blizzard1, end, start)
    val (steps3, _)         = fewestMinutesFromStartToEnd(m, n)(blizzard2, start, end)
    steps1 + steps2 + steps3
  }

  private def forwardMinute(m: Int, n: Int)(positions: Set[Pos], prevBlizzard: Blizzard, minute: Int) = {
    val blizzard     = updateBlizzard(m, n)(prevBlizzard)
    val newPositions = positions.flatMap { case (r, c) => neighbours(m, n)(blizzard.keySet, r, c) }
    (newPositions, blizzard, minute + 1)
  }

  private def updateBlizzard(m: Int, n: Int)(blizzard: Blizzard) =
    blizzard.foldLeft(Map.empty[Pos, Seq[Dir]].withDefaultValue(Seq.empty)) { case (acc, ((r, c), dirs)) =>
      dirs.foldLeft(acc) { case (acc, dir @ (dr, dc)) =>
        val pos = (math.floorMod(r + dr, m + 1), math.floorMod(c + dc, n + 1))
        acc.updated(pos, acc(pos) :+ dir)
      }
    }

  private val directions = Map('>' -> (0, 1), 'v' -> (1, 0), '<' -> (0, -1), '^' -> (-1, 0))

  private def neighbours(m: Int, n: Int)(occupied: Set[Pos], r: Int, c: Int) =
    Set(
      Option.when(!occupied.contains((r, c)))((r, c)),
      Option.when(r == m && c == n)((m + 1, n)),
      Option.when(r == 0 && c == 0)((-1, 0))
    ).flatten ++ directions.values
      .map { case (dr, dc) => (r + dr, c + dc) }
      .filter { case nei @ (nr, nc) => nr >= 0 && nr <= m && nc >= 0 && nc <= n && !occupied.contains(nei) }

  private def mapBlizzard(m: Int, n: Int)(input: Seq[String]) =
    for {
      r <- 0 to m
      c <- 0 to n
      if directions.keySet.contains(input(r)(c))
    } yield (r, c) -> Seq(directions(input(r)(c)))

}
