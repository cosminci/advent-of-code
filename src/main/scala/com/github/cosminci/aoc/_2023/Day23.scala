package com.github.cosminci.aoc._2023

import com.github.cosminci.aoc.utils
import monocle.syntax.all._

import scala.util.chaining._

object Day23 {

  // Requires increased JVM stack size e.g. `-Xss10m`
  def main(args: Array[String]): Unit = {
    val grid = utils.loadInputAsStrings("2023/day23.txt")

    println(s"Part 1: ${longestHikeWithSlopes(grid)}")
    println(s"Part 2: ${longestHikeWithoutSlopes(grid)}")
  }

  final case class Pos(r: Int, c: Int)
  type Graph = Map[Pos, Seq[(Pos, Int)]]

  def longestHikeWithSlopes(grid: Seq[String]): Int =
    longestHike(grid, neighboursFn = neighboursGivenSlopes(grid))

  def longestHikeWithoutSlopes(grid: Seq[String]): Int =
    buildGraph(grid).pipe(graph => longestHike(grid, neighboursFn = graph(_)))

  private def longestHike(grid: Seq[String], neighboursFn: Pos => Seq[(Pos, Int)]) = {
    val start = Pos(0, grid.head.indexOf('.'))
    val end   = Pos(grid.length - 1, grid.last.indexOf('.'))

    def dfs(p: Pos, steps: Int, visited: Set[Pos]): Int =
      if (p == end) steps
      else neighboursFn(p)
        .filterNot { case (nei, _) => visited.contains(nei) }
        .map { case (nei, dist) => dfs(nei, steps + dist, visited + nei) }
        .maxOption.getOrElse(Int.MinValue)

    dfs(start, steps = 0, visited = Set(start))
  }

  private def buildGraph(grid: Seq[String]) = {
    val start      = Pos(0, grid.head.indexOf('.'))
    val end        = Pos(grid.length - 1, grid.last.indexOf('.'))
    val vertices   = (findJunctions(grid) :+ start :+ end).toSet
    val emptyGraph = Map.empty[Pos, Seq[(Pos, Int)]].withDefaultValue(Seq.empty)
    vertices.foldLeft(emptyGraph)((graph, v) => computeDistToNei(v, graph, vertices, grid))
  }

  private def computeDistToNei(v: Pos, graph: Graph, vertices: Set[Pos], grid: Seq[String]) = {
    def dfs(toVisit: Seq[(Pos, Int)], visited: Set[Pos], graph: Graph): Graph =
      toVisit match {
        case Seq() => graph
        case (curr, steps) +: tail =>
          neighbours(curr, grid)
            .filterNot(visited.contains)
            .foldLeft(tail, visited, graph) { case ((toVisit, visited, graph), nei) =>
              if (vertices.contains(nei)) (toVisit, visited + nei, graph.updated(v, graph(v) :+ (nei, steps + 1)))
              else (toVisit :+ (nei, steps + 1), visited + nei, graph)
            }.pipe((dfs _).tupled)
      }
    dfs(toVisit = Seq((v, 0)), visited = Set(v), graph)
  }

  private def findJunctions(grid: Seq[String]) =
    grid.indices
      .flatMap(r => grid(r).indices.map(c => Pos(r, c)))
      .filter(p => grid(p.r)(p.c) != '#' && neighbours(p, grid).length > 2)

  private def neighboursGivenSlopes(grid: Seq[String])(p: Pos) =
    if (grid(p.r)(p.c) != '.' && grid(p.r)(p.c) != '#') Seq(slopeEnd(p, grid(p.r)(p.c)) -> 1)
    else neighbours(p, grid).map(_ -> 1)

  private def neighbours(p: Pos, grid: Seq[String]) =
    Seq(Pos(p.r - 1, p.c), Pos(p.r + 1, p.c), Pos(p.r, p.c - 1), Pos(p.r, p.c + 1))
      .filter { case Pos(r, c) => r >= 0 && r < grid.length && c >= 0 && c < grid.head.length }
      .filterNot { case Pos(r, c) => grid(r)(c) == '#' }

  private def slopeEnd(p: Pos, slope: Char) = slope match {
    case '>' => p.focus(_.c).modify(_ + 1)
    case 'v' => p.focus(_.r).modify(_ + 1)
  }

}
