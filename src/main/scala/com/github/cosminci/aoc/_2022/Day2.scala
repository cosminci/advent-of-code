package com.github.cosminci.aoc._2022

import com.github.cosminci.aoc.utils

object Day2 {

  def main(args: Array[String]): Unit = {
    val input = utils
      .loadInputAsStrings("2022/day2.txt")
      .map(round => (round.head - 'A', round.last - 'X'))

    println(s"Part 1: ${score1(input)}")
    println(s"Part 2: ${score2(input)}")
  }

  def score1(input: Seq[(Int, Int)]): Int =
    input.map { case (opponent, player) => player + 1 + movesScore(player, opponent) }.sum

  private def movesScore(player: Int, opponent: Int) =
    if (player == opponent) 3
    else if (math.floorMod(player - opponent, 3) == 1) 6
    else 0

  def score2(input: Seq[(Int, Int)]): Int =
    input.map { case (opponent, outcome) => playerMove(opponent, outcome) + 1 + outcome * 3 }.sum

  private def playerMove(opponent: Int, outcome: Int): Int =
    if (outcome == 1) opponent
    else if (outcome == 0) math.floorMod(opponent - 1, 3)
    else (opponent + 1) % 3

}
