package com.github.cosminci.aoc._2024

import com.github.cosminci.aoc.utils

import scala.util.chaining.scalaUtilChainingOps

object Day2 {

  def main(args: Array[String]): Unit = {
    val reports = parseInput(utils.loadInputAsStrings("2024/day2.txt"))

    println(s"Part 1: ${safeReports(reports)}")
    println(s"Part 2: ${safeReportsWithDampener(reports)}")
  }

  def safeReports(reports: Seq[Seq[Int]]) =
    reports.count(isSafe)

  def safeReportsWithDampener(reports: Seq[Seq[Int]]) = {
    val (safe, unsafe) = reports.partition(isSafe)
    safe.size + unsafe.count(canMakeSafe)
  }

  private def isSafe(r: Seq[Int]) =
    r.sliding(3).forall { case Seq(a, b, c) => isValidTuple(a, b, c) }

  private def canMakeSafe(r: Seq[Int]) =
    r.zipWithIndex
      .sliding(3)
      .collectFirst { case Seq((a, i), (b, j), (c, k)) if !isValidTuple(a, b, c) => (i, j, k) }
      .exists { case (i, j, k) => Seq(i, j, k).exists(i => isSafe(r.take(i) ++ r.drop(i + 1))) }

  private def isValidTuple(a: Int, b: Int, c: Int) =
    ((a < b && b < c) || (a > b && b > c)) &&
      (a - b).abs.min((b - c).abs) >= 1 &&
      (a - b).abs.max((b - c).abs) <= 3

  private def parseInput(input: Seq[String]) =
    input.map(_.split(' ').map(_.toInt).toSeq)

}
