package com.github.cosminci.aoc._2023

import com.github.cosminci.aoc.utils

object Day7 {

  def main(args: Array[String]): Unit = {
    val input = utils.loadInputAsStrings("2023/day7.txt")
    val hands = parseInput(input)

    println(s"Part 1: ${totalWinnings(hands, jokers = false)}")
    println(s"Part 2: ${totalWinnings(hands, jokers = true)}")
  }

  def totalWinnings(hands: Seq[(String, Int)], jokers: Boolean): Int =
    hands
      .sortWith { case ((cards1, _), (cards2, _)) => lowerThan(cards1, cards2, jokers) }
      .zipWithIndex
      .map { case ((_, bid), index) => bid * (index + 1) }
      .sum

  private def lowerThan(cards1: String, cards2: String, jokers: Boolean): Boolean =
    if (handType(cards1, jokers) > handType(cards2, jokers)) false
    else if (handType(cards1, jokers) < handType(cards2, jokers)) true
    else cards1.map(normalize(_, jokers)).mkString < cards2.map(normalize(_, jokers)).mkString

  private def handType(cards: String, jokers: Boolean): Int = {
    val counts = cards.groupMapReduce(identity)(_ => 1)(_ + _)
    if (jokers) handType(adjustCounts(counts).values.toSeq) else handType(counts.values.toSeq)
  }

  private def handType(counts: Seq[Int]): Int =
    if (counts.contains(5)) 7
    else if (counts.contains(4)) 6
    else if (counts.contains(3) && counts.contains(2)) 5
    else if (counts.contains(3)) 4
    else if (counts.count(_ == 2) == 2) 3
    else if (counts.contains(2)) 2
    else 1

  private def adjustCounts(counts: Map[Char, Int]): Map[Char, Int] = {
    val jokers                  = counts.getOrElse('J', 0)
    val (maxCard, maxCardCount) = counts.removed('J').maxByOption { case (_, count) => count }.getOrElse(('J', 0))
    counts.removed('J').updated(maxCard, maxCardCount + jokers)
  }

  private def normalize(card: Char, jokers: Boolean): Char =
    card match {
      case 'A' => 'Z'
      case 'K' => 'Y'
      case 'Q' => 'X'
      case 'J' => if (jokers) '1' else 'W'
      case 'T' => 'V'
      case _   => card
    }

  private def parseInput(input: Seq[String]): Seq[(String, Int)] =
    input.map(_.split(' ')).map { case Array(cards, bid) => (cards, bid.toInt) }

}
