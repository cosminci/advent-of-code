package com.github.cosminci.aoc._2018

import com.github.cosminci.aoc.utils

object Day9 {

  def main(args: Array[String]): Unit = {
    val (players, lastMarble) = parseInput(utils.loadInputAsStrings("2018/day9.txt").head)

    println(s"Part 1: ${winningScore(players, lastMarble)}")
    println(s"Part 2: ${winningScore(players, lastMarble * 100)}")
  }

  final case class MarbleRing(init: List[Int] = Nil, curr: Int, tail: List[Int] = Nil) {
    def insert(value: Int): MarbleRing = MarbleRing(init, value, curr :: tail)

    def remove: (Int, MarbleRing) = tail match {
      case head :: tail => (curr, MarbleRing(init, head, tail))
      case Nil          => (curr, MarbleRing(init.tail, init.head, Nil))
    }

    def rotate(n: Int): MarbleRing =
      if (n >= 0) Iterator.iterate(this)(_.rotateCW).drop(n).next()
      else Iterator.iterate(this)(_.rotateCCW).drop(n.abs).next()

    def rotateCW: MarbleRing = tail match {
      case head :: tail => MarbleRing(curr :: init, head, tail)
      case Nil =>
        init.reverse match {
          case head :: init => MarbleRing(List(curr), head, init)
          case Nil          => this
        }
    }

    def rotateCCW: MarbleRing = init match {
      case head :: init => MarbleRing(init, head, curr :: tail)
      case Nil =>
        tail.reverse match {
          case head :: tail => MarbleRing(tail, head, List(curr))
          case Nil          => this
        }
    }
  }

  def winningScore(players: Int, lastMarble: Int): Long = {
    @annotation.tailrec
    def dfs(turn: Int, player: Int, marbles: MarbleRing, scores: Map[Int, Long]): Long =
      if (turn > lastMarble) scores.values.max
      else if (turn % 23 == 0) {
        val (removed, newMarbles) = marbles.rotate(-7).remove
        val newScores             = scores.updated(player, scores(player) + turn + removed)
        dfs(turn + 1, (player + 1) % players, newMarbles, newScores)
      } else {
        val newMarbles = marbles.rotate(2).insert(turn)
        dfs(turn + 1, (player + 1) % players, newMarbles, scores)
      }

    dfs(turn = 1, player = 0, MarbleRing(curr = 0), scores = (0 until players).map(_ -> 0L).toMap)
  }

  private def parseInput(input: String) = input match {
    case s"$players players; last marble is worth $points points" => (players.toInt, points.toInt)
  }

}
