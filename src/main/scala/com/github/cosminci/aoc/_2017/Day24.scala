package com.github.cosminci.aoc._2017

import com.github.cosminci.aoc.utils

object Day24 {

  def main(args: Array[String]): Unit = {
    val components = parseInput(utils.loadInputAsStrings("2017/day24.txt"))

    println(s"Part 1: ${strongestBridge(components)}")
    println(s"Part 2: ${strongestLongestBridge(components)}")
  }

  final case class BridgeStats(len: Int, str: Int)

  def strongestBridge(components: Set[(Int, Int)]): Int =
    bridgeStrength(components, maxByFn = stats => stats.str)

  def strongestLongestBridge(components: Set[(Int, Int)]): Int =
    bridgeStrength(components, maxByFn = stats => (stats.len, stats.str))

  private def bridgeStrength[A: Ordering](components: Set[(Int, Int)], maxByFn: BridgeStats => A) = {
    def dfs(need: Int, components: Set[(Int, Int)], bridge: Seq[(Int, Int)]): BridgeStats =
      components.filter { case (a, b) => a == need || b == need } match {
        case Seq() => stats(bridge)
        case candidates =>
          candidates
            .map { case (a, b) => dfs(if (a == need) b else a, components - (a -> b), bridge :+ (a -> b)) }
            .maxByOption(maxByFn).getOrElse(stats(bridge))
      }

    dfs(need = 0, components, bridge = Seq.empty).str
  }

  private def stats(bridge: Seq[(Int, Int)]) =
    BridgeStats(bridge.length, bridge.map { case (a, b) => a + b }.sum)

  private def parseInput(input: Seq[String]) =
    input
      .map { case s"$a/$b" => (a.toInt, b.toInt) }
      .map { case (a, b) => (a.min(b), a.max(b)) }
      .toSet

}
