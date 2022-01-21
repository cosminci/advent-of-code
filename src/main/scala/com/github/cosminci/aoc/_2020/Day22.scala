package com.github.cosminci.aoc._2020

import cats.syntax.either._
import com.github.cosminci.aoc.utils

import scala.annotation.tailrec

object Day22 {

  def main(args: Array[String]): Unit = {
    val input = utils.loadInputAsStrings("2020/day22.txt")

    val cards            = input.filter(s => s != "" && !s.startsWith("Player")).map(_.toInt)
    val (cards1, cards2) = cards.splitAt(cards.length / 2)

    println(s"Part 1: ${winningScoreForNormalCombat(cards1, cards2)}")
    println(s"Part 2: ${winningScoreForRecursiveCombat(cards1, cards2)}")
  }

  type Cards  = Seq[Int]
  type Result = Either[Cards, Cards]

  def winningScoreForNormalCombat(cards1: Cards, cards2: Cards): Int =
    score(playCombat(cards1, cards2))

  def winningScoreForRecursiveCombat(cards1: Cards, cards2: Cards): Int =
    score(playRecursiveCombat(cards1, cards2, Set.empty).fold(l => l, r => r))

  private def score(cards: Cards): Int =
    cards.indices.foldLeft(0)((acc, i) => acc + (i + 1) * cards(cards.length - i - 1))

  @tailrec
  private def playCombat(cards1: Cards, cards2: Cards): Cards =
    (cards1, cards2) match {
      case (Seq(), _) => cards2
      case (_, Seq()) => cards1
      case (head1 +: tail1, head2 +: tail2) =>
        if (head1 > head2) playCombat(tail1 ++ Seq(head1, head2), tail2)
        else playCombat(tail1, tail2 ++ Seq(head2, head1))
    }

  private def playRecursiveCombat(cards1: Cards, cards2: Cards, visited: Set[(Cards, Cards)]): Result = {
    if (visited.contains((cards1, cards2))) cards1.asLeft
    else
      (cards1, cards2) match {
        case (Seq(), _) => cards2.asRight
        case (_, Seq()) => cards1.asLeft
        case (head1 +: tail1, head2 +: tail2) =>
          val player1Wins =
            if (tail1.length < head1 || tail2.length < head2) head1 > head2
            else playRecursiveCombat(tail1.take(head1), tail2.take(head2), Set.empty).isLeft

          val newCards1 = if (player1Wins) tail1 ++ Seq(head1, head2) else tail1
          val newCards2 = if (player1Wins) tail2 else tail2 ++ Seq(head2, head1)
          playRecursiveCombat(newCards1, newCards2, visited + ((cards1, cards2)))
      }
  }
}
