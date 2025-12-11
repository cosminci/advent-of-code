package com.github.cosminci.aoc._2025

import com.github.cosminci.aoc.utils.loadInputAsStrings

import scala.collection.mutable

object Day11 {

  def main(args: Array[String]): Unit = {
    val graph = parseInput(loadInputAsStrings("2025/day11.txt"))

    println(s"Part 1: ${countYouToOutPaths(graph)}")
    println(s"Part 2: ${countSvrToOutPaths(graph)}")
  }

  def countYouToOutPaths(graph: Map[String, Seq[String]]): Int = {
    val mem = mutable.Map.empty[String, Int]
    def dfs(curr: String): Int = mem.getOrElseUpdate(curr, 
      if (curr == "out") 1 else graph(curr).map(dfs).sum
    )
    dfs("you")
  }

  def countSvrToOutPaths(graph: Map[String, Seq[String]]): Long = {
    val bits = Map("dac" -> 1, "fft" -> 2)
    val mem = mutable.Map.empty[(String, Int), Long]
    def dfs(curr: String, seen: Int): Long = mem.getOrElseUpdate((curr, seen),
      if (curr == "out") if (seen == 3) 1 else 0
      else graph(curr).map(nei => dfs(nei, seen | bits.getOrElse(nei, 0))).sum
    )
    dfs("svr", seen = 0)
  }

  private def parseInput(input: Seq[String]) =
    input.map { line =>
      val Array(source, targets) = line.split(": ")
      source -> targets.split(' ').toSeq
    }.toMap

}
