package com.github.cosminci.aoc._2024

import com.github.cosminci.aoc.utils

import scala.util.chaining.scalaUtilChainingOps

object Day3 {

  def main(args: Array[String]): Unit = {
    val reports = utils.loadInputAsStrings("2024/day3.txt").mkString

    println(s"Part 1: ${multiplicationsResult(reports)}")
    println(s"Part 2: ${multiplicationsResultWithActivation(reports)}")
  }

  def multiplicationsResult(memory: String): Long =
    "mul\\((\\d+),(\\d+)\\)".r
      .findAllIn(memory).matchData
      .map(regexMatch => regexMatch.group(1).toLong * regexMatch.group(2).toLong)
      .sum

  def multiplicationsResultWithActivation(memory: String): Long =
    "(mul\\((\\d+),(\\d+)\\)|don't\\(\\)|do\\(\\))".r
      .findAllIn(memory).matchData
      .foldLeft(0L, true) { case ((result, active), regexMatch) =>
        regexMatch.matched match {
          case "do()"       => (result, true)
          case "don't()"    => (result, false)
          case _ if !active => (result, active)
          case _ if active =>
            (result + regexMatch.group(2).toLong * regexMatch.group(3).toLong, active)
        }
      }.pipe { case (result, _) => result }

}
