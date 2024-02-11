package com.github.cosminci.aoc._2018

import com.github.cosminci.aoc.utils

object Day18 {

  def main(args: Array[String]): Unit = {
    val grid = utils.loadInputAsStrings("2018/day18.txt").map(_.toVector).toVector

    println(s"Part 1: ${resourceValueAfterTurns(grid, totalTurns = 10)}")
    println(s"Part 2: ${resourceValueAfterTurns(grid, totalTurns = 1_000_000_000)}")
  }

  type Grid = Seq[Seq[Char]]

  def resourceValueAfterTurns(initialGrid: Grid, totalTurns: Int): Int = {
    @annotation.tailrec
    def dfs(turn: Int, grid: Grid, seen: Map[Grid, Int]): Int =
      if (turn == totalTurns) resourceValue(grid)
      else seen.get(grid) match {
        case None => dfs(turn + 1, updateGrid(grid), seen.updated(grid, turn))
        case Some(cycleStartTurn) =>
          val remainingTurns = (totalTurns - turn) % (turn - cycleStartTurn)
          resourceValue(Iterator.iterate(grid)(updateGrid).drop(remainingTurns).next())
      }

    dfs(turn = 0, initialGrid, Map.empty)
  }

  private def resourceValue(grid: Grid) =
    grid.flatten.count(_ == '|') * grid.flatten.count(_ == '#')

  private def updateGrid(grid: Grid) =
    for ((row, r) <- grid.zipWithIndex)
      yield for ((tile, c) <- row.zipWithIndex)
        yield {
          val nei = neighbours(r, c, grid)
          tile match {
            case '.' => if (nei('|') >= 3) '|' else '.'
            case '|' => if (nei('#') >= 3) '#' else '|'
            case '#' => if (nei('#') >= 1 && nei('|') >= 1) '#' else '.'
          }
        }

  private def neighbours(r: Int, c: Int, grid: Grid) =
    Seq((-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1))
      .map { case (dr, dc) => (r + dr, c + dc) }
      .filter { case (nr, nc) => nr >= 0 && nr < grid.length && nc >= 0 && nc < grid(nr).length }
      .map { case (nr, nc) => grid(nr)(nc) }
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .withDefaultValue(0)

}
