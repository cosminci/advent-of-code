package com.github.cosminci.aoc._2023

import com.github.cosminci.aoc.utils

object Day9 {

  def main(args: Array[String]): Unit = {
    val sequences = utils.loadInputAsStrings("2023/day9.txt").map(_.split(' ').toSeq.map(_.toInt))

    println(s"Part 1: ${extrapolateEndings(sequences)}")
    println(s"Part 2: ${extrapolateStarts(sequences)}")
  }

  def extrapolateEndings(sequences: Seq[Seq[Int]]): Int =
    sequences.map(buildDiffs(_).map(_.last).sum).sum

  def extrapolateStarts(sequences: Seq[Seq[Int]]): Int =
    sequences.map(buildDiffs(_).map(_.head).foldRight(0)(_ - _)).sum

  private def buildDiffs(seq: Seq[Int]): Seq[Seq[Int]] =
    if (seq.forall(_ == 0)) Seq(seq)
    else seq +: buildDiffs(seq.sliding(2).map(w => w.last - w.head).toSeq)

}
