package com.github.cosminci.aoc._2024

import com.github.cosminci.aoc.utils

object Day5 {

  def main(args: Array[String]): Unit = {
    val order   = parseOrder(utils.loadInputAsStrings("2024/day5-order.txt"))
    val updates = parseUpdates(utils.loadInputAsStrings("2024/day5-updates.txt"))

    println(s"Part 1: ${middleCorrectPageSum(updates, order)}")
    println(s"Part 2: ${middleIncorrectPageSum(updates, order)}")
  }

  def middleCorrectPageSum(updates: Seq[Seq[Int]], afterToBefore: Map[Int, Seq[Int]]): Int =
    updates
      .filter(hasCorrectOrder(_, afterToBefore))
      .map(update => update(update.length / 2))
      .sum

  def middleIncorrectPageSum(updates: Seq[Seq[Int]], afterToBefore: Map[Int, Seq[Int]]): Int =
    updates
      .filter(!hasCorrectOrder(_, afterToBefore))
      .map(orderPages(_, afterToBefore))
      .map(update => update(update.length / 2))
      .sum

  private def hasCorrectOrder(update: Seq[Int], afterToBefore: Map[Int, Seq[Int]]) =
    update.sliding(2).forall { case Seq(before, after) => afterToBefore(after).contains(before) }

  private def orderPages(update: Seq[Int], afterToBefore: Map[Int, Seq[Int]]) =
    update.sorted((after: Int, before: Int) => if (afterToBefore(after).contains(before)) -1 else 1)

  private def parseUpdates(input: Seq[String]) =
    input.map(_.split(',').map(_.toInt).toSeq)

  private def parseOrder(input: Seq[String]) =
    input.foldLeft(Map.empty[Int, Seq[Int]].withDefaultValue(Seq.empty)) { case (afterToBefore, s"$before|$after") =>
      afterToBefore.updated(after.toInt, afterToBefore(after.toInt) :+ before.toInt)
    }

}
