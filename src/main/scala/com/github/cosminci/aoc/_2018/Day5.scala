package com.github.cosminci.aoc._2018

import com.github.cosminci.aoc.utils

object Day5 {

  def main(args: Array[String]): Unit = {
    val string = utils.loadInputAsStrings("2018/day5.txt").head

    println(s"Part 1: ${unitsLeftAfterReactions(string)}")
    println(s"Part 2: ${shortestPolymerAfterReactions(string)}")
  }

  def unitsLeftAfterReactions(s: String): Int =
    s.foldLeft(List.empty[Char]) { case (acc, curr) =>
      acc match {
        case Seq()        => curr +: acc
        case prev +: tail => if ((curr - prev).abs == 32) tail else curr +: acc
      }
    }.size

  def shortestPolymerAfterReactions(s: String): Int =
    s.map(_.toLower)
      .distinct
      .map(cleanPolymer(s, _))
      .map(unitsLeftAfterReactions)
      .min

  private def cleanPolymer(s: String, char: Char) =
    s.filterNot(ch => Seq(0, 32).contains(char - ch))

}
