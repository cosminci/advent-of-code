package com.github.cosminci.aoc._2021

import scala.collection.mutable

object Day21 {
  def main(args: Array[String]): Unit = {
    println(s"Part I: ${playDeterministic(p1Start = 7, p2Start = 1)}")
    println(s"Part II: ${playDirac(p1Start = 7, p2Start = 1)}")
  }

  def playDeterministic(p1Start: Int, p2Start: Int): Int = {
    val nextDice = (d: Int) => Option.when(d + 1 <= 100)(d + 1).getOrElse(1)

    val playRound = (score1: Int, score2: Int, pos1: Int, pos2: Int, dice: Int) => {
      val rolled = Iterator.iterate(dice)(nextDice).take(3).toSeq
      val newPos = rolled.foldLeft(pos1)((p, r) => (p + r) % 10)
      (score2, score1 + newPos + 1, pos2, newPos, nextDice(rolled.last))
    }

    val rounds = Iterator
      .iterate(0, 0, p1Start - 1, p2Start - 1, 1)(playRound.tupled)
      .takeWhile { case (score1, score2, _, _, _) => score1 < 1000 && score2 < 1000 }
      .toSeq

    val (score1, score2, _, _, _) = rounds.last
    score1.min(score2) * rounds.size * 3
  }

  def playDirac(p1Start: Int, p2Start: Int): Long = {
    val rolls = for {
      r1 <- 1 to 3
      r2 <- 1 to 3
      r3 <- 1 to 3
    } yield r1 + r2 + r3

    val mem = mutable.Map.empty[(Int, Int, Int, Int), (Long, Long)]
    def dfs(pos1: Int, score1: Int, pos2: Int, score2: Int): (Long, Long) =
      mem.getOrElseUpdate((pos1, score1, pos2, score2), {
        if (score1 >= 21) (1L, 0L)
        else if (score2 >= 21) (0L, 1L)
        else rolls.foldLeft(0L, 0L) { case ((wins1, wins2), rollSum) =>
          val newPos1   = (pos1 + rollSum) % 10
          val newScore1 = score1 + newPos1 + 1
          val (w2, w1) = dfs(pos2, score2, newPos1, newScore1)
          (wins1 + w1, wins2 + w2)
        }
      })

    val (wins1, wins2) = dfs(p1Start - 1, 0, p2Start - 1, 0)
    wins1.max(wins2)
  }
}
