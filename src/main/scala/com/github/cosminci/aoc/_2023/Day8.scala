package com.github.cosminci.aoc._2023

import com.github.cosminci.aoc.utils

object Day8 {

  def main(args: Array[String]): Unit = {
    val input            = utils.loadInputAsStrings("2023/day8.txt")
    val (pattern, graph) = parseInput(input)

    println(s"Part 1: ${stepsToExit(pattern, graph)}")
    println(s"Part 2: ${stepsToExitAsGhost(pattern, graph)}")
  }

  def stepsToExit(pattern: Seq[Int], graph: Map[String, Seq[String]]): Int = {
    @annotation.tailrec
    def dfs(i: Int, curr: String): Int =
      if (curr == "ZZZ") i
      else dfs(i + 1, graph(curr)(pattern(i % pattern.length)))

    dfs(i = 0, curr = "AAA")
  }

  def stepsToExitAsGhost(pattern: IndexedSeq[Int], graph: Map[String, Seq[String]]): Long = {
    @annotation.tailrec
    def dfs(i: Int, curr: String): Long =
      if (curr.last == 'Z') i
      else dfs(i + 1, graph(curr)(pattern(i % pattern.length)))

    graph.keys.filter(_.last == 'A').map(dfs(i = 0, _)).reduce(utils.lcm)
  }

  private def parseInput(input: Seq[String]) = {
    val pattern = input.head.map(ch => if (ch == 'L') 0 else 1)
    val graph = input
      .drop(2)
      .flatMap("(\\w+) = \\((\\w+), (\\w+)\\)".r.findFirstMatchIn)
      .map(m => m.group(1) -> Seq(m.group(2), m.group(3)))
      .toMap

    (pattern, graph)
  }

}
