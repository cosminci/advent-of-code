package com.github.cosminci.aoc._2022

import com.github.cosminci.aoc.utils

import scala.collection.BitSet
import scala.util.chaining._

object Day16 {

  def main(args: Array[String]): Unit = {
    val input = utils.loadInputAsStrings("2022/day16.txt")

    println(s"Part 1: ${maxPressureReleased(input)}")
    println(s"Part 2: ${maxPressureReleasedWithHelp(input)}")
  }

  def maxPressureReleased(input: Seq[String]): Int =
    maxPressurePerValveSet(input, totalTime = 30).values.max

  def maxPressureReleasedWithHelp(input: Seq[String]): Int =
    maxPressurePerValveSet(input, totalTime = 26).pipe { results =>
      for {
        (valveSet1, pressureReleased1) <- results
        (valveSet2, pressureReleased2) <- results
        if (valveSet1 intersect valveSet2).isEmpty
      } yield pressureReleased1 + pressureReleased2
    }.max

  final case class State(time: Int, pos: Int, toVisit: Set[Int], visited: BitSet, pressureAcc: Int)

  private def maxPressurePerValveSet(input: Seq[String], totalTime: Int) = {
    val (start, flowRates, pairwiseDist) = parseInput(input)

    def dfs(state: State, res: Map[BitSet, Int]): Map[BitSet, Int] =
      state.toVisit.foldLeft(res.updated(state.visited, res(state.visited) max state.pressureAcc)) { (res, next) =>
        val timeLeft = state.time - pairwiseDist(state.pos)(next) - 1
        if (timeLeft <= 0) res
        else {
          val newPressureAcc = state.pressureAcc + timeLeft * flowRates(next)
          val newState       = State(timeLeft, next, state.toVisit - next, state.visited + next, newPressureAcc)
          dfs(newState, res)
        }
      }

    val targetValves = flowRates.collect { case (valve, flowRate) if flowRate > 0 => valve }.toSet
    val initialState = State(totalTime, pos = start, toVisit = targetValves, visited = BitSet.empty, pressureAcc = 0)
    dfs(initialState, res = Map.empty.withDefaultValue(0))
  }

  private def findPairwiseDistances(graph: Map[Int, Seq[Int]]) =
    graph.keySet.map { start =>
      Iterator
        .iterate((Seq(start), Map(start -> 0))) { case (curr +: unseen, seen) => step(graph, curr, unseen, seen) }
        .dropWhile { case (toVisit, _) => toVisit.nonEmpty }
        .next()
        .pipe { case (_, visited) => start -> visited }
    }.toMap

  private def step(graph: Map[Int, Seq[Int]], curr: Int, unseen: Seq[Int], seen: Map[Int, Int]) =
    graph(curr).filterNot(seen.contains).foldLeft(unseen, seen) { case ((unseen, seen), next) =>
      (unseen :+ next, seen.updated(next, seen(curr) + 1))
    }

  private def parseInput(input: Seq[String]) = {
    val idMap     = input.zipWithIndex.map { case (s"Valve $name has $_", i) => name -> i }.toMap
    val flowRates = input.map { case s"Valve $name has flow rate=$flowRate; $_" => idMap(name) -> flowRate.toInt }.toMap
    val graph = input.map { case s"Valve $valve $_ to valve$_ $tunnels" =>
      idMap(valve) -> tunnels.split(", ").map(idMap).toSeq
    }.toMap

    (idMap("AA"), flowRates, findPairwiseDistances(graph))
  }

}
