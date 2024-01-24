package com.github.cosminci.aoc._2017

import com.github.cosminci.aoc.utils

import scala.util.chaining._

object Day12 {

  def main(args: Array[String]): Unit = {
    val graph = parseInput(utils.loadInputAsStrings("2017/day12.txt"))

    println(s"Part 1: ${programsInGroupWithZero(graph)}")
    println(s"Part 2: ${programGroupCount(graph)}")
  }

  def programsInGroupWithZero(graph: Map[Int, Array[Int]]): Int =
    groupWithNode(graph, node = 0).size

  def programGroupCount(graph: Map[Int, Array[Int]]): Int =
    graph.keys.foldLeft(Set.empty[Int], 0) { case ((visited, groupCount), node) =>
      if (visited.contains(node)) (visited, groupCount)
      else (visited ++ groupWithNode(graph, node), groupCount + 1)
    }.pipe { case (_, groupCount) => groupCount }

  private def groupWithNode(graph: Map[Int, Array[Int]], node: Int) = {
    @annotation.tailrec
    def dfs(toVisit: Seq[Int], visited: Set[Int]): Set[Int] =
      toVisit match {
        case Seq() => visited
        case curr +: remaining =>
          dfs(remaining ++ graph(curr).filterNot(visited.contains), visited + curr)
      }
    dfs(toVisit = Seq(node), visited = Set.empty)
  }

  private def parseInput(lines: Seq[String]) =
    lines.map { case s"$id <-> $neighbors" => id.toInt -> neighbors.split(", ").map(_.toInt) }.toMap

}
