package com.github.cosminci.aoc._2025

import com.github.cosminci.aoc.utils.loadInputAsStrings

import scala.util.chaining.scalaUtilChainingOps

object Day2 {

  def main(args: Array[String]): Unit = {
    val input = parseInput(loadInputAsStrings("2025/day2.txt").head)

    println(s"Part 1: ${invalidIdSum(input, isInvalidByHalves)}")
    println(s"Part 2: ${invalidIdSum(input, isInvalidByPattern)}")
  }

  def invalidIdSum(idRanges: Seq[(Long, Long)], isInvalidFn: String => Boolean): Long =
    idRanges.map { case (l, r) => (l to r).filter(id => isInvalidFn(id.toString)).sum }.sum

  private def isInvalidByHalves(id: String) =
    id.splitAt(id.length / 2).pipe { case (fh, sh) => fh == sh }

  private def isInvalidByPattern(id: String) =
    (1 to id.length / 2).exists { i =>
      id.grouped(i).sliding(2).forall { case Seq(a, b) => a == b }
    }

  private def parseInput(input: String) =
    input
      .split(',')
      .map(_.split('-').map(_.toLong))
      .map { case Array(a, b) => (a, b) }

}
