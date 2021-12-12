package com.github.cosminci.aoc._2021

import com.github.cosminci.aoc.utils

import scala.collection.mutable

object Day12 {
  def main(args: Array[String]): Unit = {
    val adjList = utils
      .loadInputAsStrings("2021/day12.txt")
      .foldLeft(Map.empty[String, Seq[String]].withDefaultValue(Seq.empty)) { (adjList, line) =>
        val Array(node1, node2) = line.split('-')
        adjList.updated(node1, adjList(node1) :+ node2).updated(node2, adjList(node2) :+ node1)
      }

    println(s"Part I: ${countPathsWithUniqueSmallCaves(adjList)}")
    println(s"Part II: ${countPathsWithSingleSmallCaveTwice(adjList)}")
  }

  def countPathsWithUniqueSmallCaves(adjList: Map[String, Seq[String]]): Int =
    countPaths(adjList, (visited, next) => !visited.contains(next))

  def countPathsWithSingleSmallCaveTwice(adjList: Map[String, Seq[String]]): Int =
    countPaths(adjList, (visited, n) => !visited.contains(n) || (n != "start" && visited.values.forall(_ == 1)))

  type CanVisitPredicate = (Map[String, Int], String) => Boolean

  private def countPaths(adjList: Map[String, Seq[String]], predicate: CanVisitPredicate): Int = {
    val mem = mutable.Map.empty[(String, Map[String, Int]), Int]

    def dfs(curr: String, visited: Map[String, Int]): Int =
      mem.getOrElseUpdate((curr, visited), {
        if (curr == "end") 1
        else adjList(curr).collect { case next if predicate(visited, next) =>
          dfs(next, Option.when(next.forall(_.isLower))(visited.updated(next, visited(next) + 1)).getOrElse(visited))
        }.sum
      })

    dfs("start", Map("start" -> 1).withDefaultValue(0))
  }
}
