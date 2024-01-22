package com.github.cosminci.aoc._2017

import com.github.cosminci.aoc.utils

import scala.collection.mutable
import scala.util.chaining._

object Day7 {

  def main(args: Array[String]): Unit = {
    val (graph, weights) = parseInput(utils.loadInputAsStrings("2017/day7.txt"))

    println(s"Part 1: ${findRootProgram(graph)}")
    println(s"Part 2: ${findCorrectWeight(graph, weights)}")
  }

  def findRootProgram(graph: Map[String, Seq[String]]): String = {
    val childToParent = graph.toSeq.flatMap { case (parent, children) => children.map(_ -> parent) }.toMap
    graph.keys.find(!childToParent.contains(_)).get
  }

  def findCorrectWeight(graph: Map[String, Seq[String]], weights: Map[String, Int]): Int = {
    val mem = mutable.Map.empty[String, Int]
    def calculateWeight(node: String): Int =
      mem.getOrElseUpdate(node, weights(node) + graph(node).map(calculateWeight).sum)

    @annotation.tailrec
    def dfs(node: String, expectedWeight: Int): Int = {
      if (graph(node).isEmpty) expectedWeight
      else {
        val (incorrectChildren, correctWeights) = graph(node)
          .map(child => child -> calculateWeight(child))
          .groupMap { case (_, weight) => weight } { case (child, _) => child }
          .partitionMap { case (weight, children) => Either.cond(children.length > 1, weight, children.head) }

        incorrectChildren.toSeq match {
          case Seq()               => expectedWeight - correctWeights.head * graph(node).size
          case Seq(incorrectChild) => dfs(incorrectChild, correctWeights.head)
        }
      }
    }

    dfs(node = findRootProgram(graph), expectedWeight = -1)
  }

  private def parseInput(input: Seq[String]) = {
    val rows    = input.map(parseLine)
    val graph   = rows.map { case (node, _, nei) => (node, nei) }.toMap
    val weights = rows.map { case (node, weight, _) => (node, weight) }.toMap
    (graph, weights)
  }

  private def parseLine(s: String): (String, Int, Seq[String]) = s match {
    case s"$name ($weight) -> $children" => (name, weight.toInt, children.split(", ").toSeq)
    case s"$name ($weight)"              => (name, weight.toInt, Seq.empty)
  }

}
