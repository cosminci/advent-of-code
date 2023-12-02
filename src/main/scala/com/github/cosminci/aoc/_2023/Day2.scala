package com.github.cosminci.aoc._2023

import cats.Semigroup
import cats.data.{NonEmptyList, NonEmptyMap}
import cats.parse.{Parser, Rfc5234}
import com.github.cosminci.aoc.utils

object Day2 {

  def main(args: Array[String]): Unit = {
    val input       = utils.loadInputAsStrings("2023/day2.txt")
    val gameResults = parseGameResults(input)

    println(s"Part 1: ${possibleGameIdSum(gameResults)}")
    println(s"Part 2: ${minPossibleCountsPower(gameResults)}")
  }

  private val limits = Map("red" -> 12, "green" -> 13, "blue" -> 14)

  def possibleGameIdSum(gameResults: Map[Int, NonEmptyList[NonEmptyMap[String, Int]]]) =
    gameResults.collect { case (id, results) if isGamePossible(results) => id }.sum

  private def isGamePossible(rounds: NonEmptyList[NonEmptyMap[String, Int]]) =
    rounds.forall { round =>
      round.toSortedMap.forall { case (color, count) => limits(color) >= count }
    }

  def minPossibleCountsPower(gameResults: Map[Int, NonEmptyList[NonEmptyMap[String, Int]]]) = {
    implicit val minCount: Semigroup[Int] = (x: Int, y: Int) => x max y
    gameResults.values.map(_.reduce.toSortedMap.values.product).sum
  }

  private def parseGameResults(input: Seq[String]) = {
    val number       = Rfc5234.digit.rep.string.map(_.toInt)
    val color        = Rfc5234.alpha.rep.string
    val colorCount   = (number ~ Rfc5234.sp ~ color).map { case ((cnt, _), clr) => clr -> cnt }
    val singleCounts = colorCount.repSep(Parser.string(", ")).map(_.toNem)
    val gameCounts   = singleCounts.repSep(Parser.string("; "))
    val gameId       = (Parser.string("Game ") ~ number ~ Parser.string(": ")).map { case ((_, id), _) => id }
    val gameResult   = gameId ~ gameCounts
    input.flatMap(s => gameResult.parseAll(s).toOption).toMap
  }

}
