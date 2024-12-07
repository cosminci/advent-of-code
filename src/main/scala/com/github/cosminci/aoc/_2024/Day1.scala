package com.github.cosminci.aoc._2024

import com.github.cosminci.aoc.utils

import scala.util.chaining.scalaUtilChainingOps

object Day1 {

  def main(args: Array[String]): Unit = {
    val (listA, listB) = parseInput(utils.loadInputAsStrings("2024/day1.txt"))

    println(s"Part 1: ${totalDistance(listA, listB)}")
    println(s"Part 2: ${similarityScore(listA, listB)}")
  }

  def similarityScore(listA: Seq[Int], listB: Seq[Int]): Int = {
    val countsB = listB.groupMapReduce(identity)(_ => 1)(_ + _)
    listA.map(a => a * countsB.getOrElse(a, 0)).sum
  }

  def totalDistance(listA: Seq[Int], listB: Seq[Int]): Int =
    listA.sorted.zip(listB.sorted).map { case (a, b) => (a - b).abs }.sum

  private val inputPattern = """(\d+) {3}(\d+)""".r
  private def parseInput(input: Seq[String]) =
    input
      .map { case inputPattern(a, b) => Seq(a.toInt, b.toInt) }
      .transpose
      .pipe { case Seq(listA, listB) => (listA, listB) }

}
