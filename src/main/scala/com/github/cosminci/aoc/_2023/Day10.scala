package com.github.cosminci.aoc._2023

import com.github.cosminci.aoc.utils

object Day10 {

  def main(args: Array[String]): Unit = {
    val grid = utils.loadInputAsStrings("2023/day10.txt")

    println(s"Part 1: ${stepsToFarthestAway(grid)}")
    println(s"Part 2: ${tilesEnclosedByLoop(grid)}")
  }

  final case class Pos(x: Int, y: Int)

  def stepsToFarthestAway(grid: Seq[String]): Int =
    findLoop(grid).length / 2

  def tilesEnclosedByLoop(grid: Seq[String]): Int = {
    val loop    = findLoop(grid)
    val loopSet = loop.toSet
    val inside  = raycastGrid(loopSet, cleanGrid(grid, loop))
    val points  = grid.indices.flatMap(x => grid(x).indices.map(y => Pos(x, y)))

    points.count(p => !loopSet.contains(p) && inside(p.x)(p.y))
  }

  private def raycastGrid(loop: Set[Pos], grid: Seq[String]) =
    grid.indices.map { x =>
      grid(x).indices.scanLeft(false) { case (inside, y) =>
        if (!loop.contains(Pos(x, y)) && grid(x)(y) != '.') inside
        else if (Set('|', 'L', 'J').contains(grid(x)(y))) !inside
        else inside
      }
    }

  private def findLoop(grid: Seq[String]) = {
    val start = findStart(grid)
    val first = neighbours(start).filter(next => isWithinGrid(next, grid) && canConnectFrom(start, next, grid)).head

    @annotation.tailrec
    def dfs(curr: Pos, prev: Pos, loop: Seq[Pos]): Seq[Pos] =
      if (curr == start) loop
      else dfs(neighbours(curr).filterNot(_ == prev).find(canConnect(curr, _, grid)).get, curr, loop :+ curr)

    dfs(curr = first, prev = start, loop = Seq(start))
  }

  private def cleanGrid(grid: Seq[String], loop: Seq[Pos]) = {
    val (last, start, first) = (loop.last, loop.head, loop.tail.head)
    val cleanStart           = fit(last, start).intersect(fit(first, start)).head
    grid.updated(start.x, grid(start.x).updated(start.y, cleanStart))
  }

  private def fit(p1: Pos, p2: Pos) = connectableShapes((p2.x - p1.x, p2.y - p1.y)).toSet

  private val connectableShapes = Map(
    (0, -1) -> Seq('-', 'L', 'F'),
    (0, 1)  -> Seq('-', 'J', '7'),
    (1, 0)  -> Seq('|', 'J', 'L'),
    (-1, 0) -> Seq('|', '7', 'F')
  )

  private def canConnect(curr: Pos, next: Pos, grid: Seq[String]) =
    canConnectFrom(next, curr, grid) && (grid(next.x)(next.y) == 'S' || canConnectFrom(curr, next, grid))

  private def canConnectFrom(curr: Pos, next: Pos, grid: Seq[String]) =
    connectableShapes((next.x - curr.x, next.y - curr.y)).contains(grid(next.x)(next.y))

  private def isWithinGrid(p: Pos, grid: Seq[String]) =
    p.x >= 0 && p.x < grid.length && p.y >= 0 && p.y < grid.head.length

  private def neighbours(p: Pos) =
    Seq(Pos(p.x - 1, p.y), Pos(p.x + 1, p.y), Pos(p.x, p.y - 1), Pos(p.x, p.y + 1))

  private def findStart(input: Seq[String]) =
    input.indices
      .flatMap(x => input(x).indices.map(y => (x, y)))
      .collectFirst { case (x, y) if input(x)(y) == 'S' => Pos(x, y) }
      .get

}
