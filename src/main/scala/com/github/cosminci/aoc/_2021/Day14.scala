package com.github.cosminci.aoc._2021

import com.github.cosminci.aoc.utils

object Day14 {
  def main(args: Array[String]): Unit = {
    val input = utils.loadInputAsStrings("2021/day14.txt")

    val template = input.head
    val rules = input
      .drop(2)
      .map(_.split(" -> "))
      .map { case Array(pattern, newPolymer) => (pattern, newPolymer.head) }
      .toMap

    println(s"Part I: ${diffBetweenOutlierPolymers(template, rules, reactionCount = 10)}")
    println(s"Part I: ${diffBetweenOutlierPolymers(template, rules, reactionCount = 40)}")
  }

  def diffBetweenOutlierPolymers(template: String, rules: Map[String, Char], reactionCount: Int): Long = {
    val initialChars = utils.counter(template).withDefaultValue(0L)
    val initialPairs = utils.counter(template.sliding(2).toSeq)

    val counts = (1 to reactionCount).foldLeft(initialChars, initialPairs) {
      case ((chars, pairs), _) =>
        pairs.foldLeft(chars, Map.empty[String, Long].withDefaultValue(0L)) {
          case ((chars, pairs), (pair, count)) =>
            val newPolymer     = rules(pair)
            val (pair1, pair2) = (s"${pair.head}$newPolymer", s"$newPolymer${pair.last}")
            val newCharCounts  = chars.updated(newPolymer, chars(newPolymer) + count)
            val newPairCounts  = pairs.updated(pair1, pairs(pair1) + count).updated(pair2, pairs(pair2) + count)
            (newCharCounts, newPairCounts)
        }
    }._1.values

    counts.max - counts.min
  }
}
