package com.github.cosminci.aoc._2020

import com.github.cosminci.aoc.utils

object Day6 {

  def main(args: Array[String]): Unit = {
    val input = utils
      .loadInputAsStrings("2020/day6.txt")
      .appended("")
      .foldLeft(Seq.empty[Seq[String]], Seq.empty[String]) { case ((complete, inProgress), line) =>
        if (line == "") (complete :+ inProgress, Seq.empty)
        else (complete, inProgress :+ line)
      }._1

    println(s"Part 1: ${sumDistinctAnswers(input)}")
    println(s"Part 2: ${sumAnswerIntersections(input)}")
  }

  private def sumDistinctAnswers(input: Seq[Seq[String]]): Int =
    input.map(_.flatten.toSet.size).sum

  private def sumAnswerIntersections(inputData: Seq[Seq[String]]): Int =
    inputData.map(_.reduce(_ intersect _).length).sum

}
