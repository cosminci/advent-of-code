package com.github.cosminci.aoc._2021

import com.github.cosminci.aoc.utils

import scala.util.Try

object Day4 {
  def main(args: Array[String]): Unit = {
    val input = utils.loadInputAsStrings("2021/day4.txt")
    val nums  = input.head.split(',').map(_.toInt).toSeq
    val boards = input.tail
      .filter(_.nonEmpty)
      .map(_.split(' ').flatMap(s => Try(s.toInt).toOption).toSeq)
      .grouped(5)
      .toSeq

    val scores = boardScores(nums, boards)
    println(s"Part I: ${scores.head}")
    println(s"Part II: ${scores.last}")
  }

  def boardScores(nums: Seq[Int], boards: Seq[Seq[Seq[Int]]]): Seq[Int] =
    nums.foldLeft(Seq.empty[Int], Set.empty[Int], boards) {
      case ((scores, marked, activeBoards), num) =>
        val newMarked     = marked + num
        val (won, active) = activeBoards.partition(b => (b ++ b.transpose).exists(_.forall(newMarked.contains)))
        val newScores     = won.map(board => num * board.flatten.filterNot(newMarked.contains).sum)
        (scores ++ newScores, newMarked, active)
    }._1
}
