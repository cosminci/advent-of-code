package com.github.cosminci.aoc._2023

import com.github.cosminci.aoc.utils

import scala.collection.mutable

object Day4 {

  def main(args: Array[String]): Unit = {
    val input        = utils.loadInputAsStrings("2023/day4.txt")
    val scratchcards = parseScratchcards(input)

    println(s"Part 1: ${scratchcardScore(scratchcards)}")
    println(s"Part 2: ${scratchcardCount(scratchcards)}")
  }

  final case class Card(winning: Array[Int], actual: Array[Int])

  def scratchcardScore(cards: Seq[Card]): Int =
    cards.map(c => math.pow(2, c.winning.intersect(c.actual).length - 1).toInt).sum

  def scratchcardCount(cards: Seq[Card]): Int = {
    val mem = mutable.Map.empty[Int, Int]
    def dfs(i: Int): Int = mem.getOrElseUpdate(i,
      if (i == cards.length) 0
      else 1 + (1 to cards(i).winning.intersect(cards(i).actual).length).map(di => dfs(i + di)).sum
    )
    cards.indices.map(dfs).sum
  }

  private def parseScratchcards(input: Seq[String]) =
    input.map { line =>
      val card    = line.replaceFirst("Card( +)(\\d+):", "").split(" \\| ")
      val winning = card(0).split(" ").filterNot(_.isEmpty).map(_.toInt)
      val actual  = card(1).split(" ").filterNot(_.isEmpty).map(_.toInt)
      Card(winning, actual)
    }

}
