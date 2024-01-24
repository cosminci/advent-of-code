package com.github.cosminci.aoc._2017

import scala.util.chaining._

object Day14 {

  def main(args: Array[String]): Unit = {
    val key = "jzgqcdpd"

    println(s"Part 1: ${usedSquareCount(key)}")
    println(s"Part 2: ${regionCount(key)}")
  }

  def usedSquareCount(key: String): Int =
    memoryGrid(key).map(_.count(_ == '1')).sum

  def regionCount(key: String): Int = {
    val grid    = memoryGrid(key)
    val toVisit = grid.indices.flatMap(r => grid.indices.collect { case c if grid(r)(c) == '1' => (r, c) })

    toVisit.foldLeft(Set.empty[(Int, Int)], 0) { case ((visited, regionCount), pos) =>
      if (visited.contains(pos)) (visited, regionCount)
      else explore(pos, grid, visited).pipe((_, regionCount + 1))
    }.pipe { case (_, regionCount) => regionCount }
  }

  private def explore(pos: (Int, Int), grid: Seq[String], visited: Set[(Int, Int)]) = {
    @annotation.tailrec
    def dfs(toVisit: Seq[(Int, Int)], visited: Set[(Int, Int)]): Set[(Int, Int)] =
      toVisit match {
        case Seq() => visited
        case (r, c) +: remaining =>
          val newToVisit = neighbours(r, c, grid).filterNot(visited.contains)
          dfs(remaining ++ newToVisit, visited ++ newToVisit)
      }
    dfs(toVisit = Seq(pos), visited)
  }

  private def neighbours(r: Int, c: Int, grid: Seq[String]) =
    Seq((-1, 0), (1, 0), (0, -1), (0, 1))
      .map { case (dr, dc) => (r + dr, c + dc) }
      .filter { case (r, c) => r >= 0 && r < grid.size && c >= 0 && c < grid(r).length && grid(r)(c) == '1' }

  private def memoryGrid(key: String) =
    (0 until 128).map { row =>
      val hash = Day10.knotHash(s"$key-$row")
      BigInt(hash, 16).toString(2).reverse.padTo(128, '0').reverse
    }

}
