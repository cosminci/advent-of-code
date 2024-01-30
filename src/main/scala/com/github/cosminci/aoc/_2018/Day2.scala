package com.github.cosminci.aoc._2018

import com.github.cosminci.aoc.utils

object Day2 {

  def main(args: Array[String]): Unit = {
    val boxIds = utils.loadInputAsStrings("2018/day2.txt")

    println(s"Part 1: ${checksum(boxIds)}")
    println(s"Part 2: ${commonLettersOfCorrectBoxes(boxIds)}")
  }

  def checksum(boxIds: Seq[String]): Int = {
    val counts = boxIds.map(s => s.groupMapReduce(identity)(_ => 1)(_ + _).values.toSet)
    val twos = counts.count(_.contains(2))
    val threes = counts.count(_.contains(3))
    twos * threes
  }

  def commonLettersOfCorrectBoxes(boxIds: Seq[String]): Option[String] =
    boxIds.combinations(2)
      .map { case Seq(id1, id2) => id1.zip(id2).collect { case (c1, c2) if c1 == c2 => c1 }.mkString }
      .collectFirst { case s if s.length == boxIds.head.length - 1 => s }

}
