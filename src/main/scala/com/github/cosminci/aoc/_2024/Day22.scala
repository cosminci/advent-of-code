package com.github.cosminci.aoc._2024

import com.github.cosminci.aoc.utils

import scala.util.chaining.scalaUtilChainingOps

object Day22 {

  def main(args: Array[String]): Unit = {
    val secretNumbers = utils.loadInputAsInts("2024/day22.txt")

    println(s"Part 1: ${finalSecretNumberSum(secretNumbers)}")
    println(s"Part 2: ${maximumBananas(secretNumbers)}")
  }

  def finalSecretNumberSum(secretNumbers: Seq[Int]): Long =
    secretNumbers.map(secretNumberSequence(_).last).foldLeft(0L)(_ + _)

  def maximumBananas(secretNumbers: Seq[Int]): Int =
    secretNumbers.foldLeft(Map.empty[Int, Int]) { case (acc, n) =>
      val priceSequence = secretNumberSequence(n).map(_ % 10)
      priceSequence.sliding(5).foldLeft(acc, Set.empty[Int]) {
        case ((acc, seen), Seq(a, b, c, d, e)) =>
          val changes = (b - a, c - b, d - c, e - d).hashCode()
          if (seen.contains(changes)) (acc, seen)
          else (acc.updated(changes, acc.getOrElse(changes, 0) + e), seen + changes)
      }.pipe { case (acc, _) => acc }
    }.values.max

  private def secretNumberSequence(n: Int) =
    Iterator.iterate(n)(nextSecretNumber).slice(1, 2001).toSeq

  private def nextSecretNumber(a: Int) = {
    val b = ((a << 6) ^ a) & 0xffffff
    val c = ((b >> 5) ^ b) & 0xffffff
    ((c << 11) ^ c) & 0xffffff
  }

}
