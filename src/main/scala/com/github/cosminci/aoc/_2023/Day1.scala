package com.github.cosminci.aoc._2023

import com.github.cosminci.aoc.utils

import scala.annotation.tailrec
import scala.util._

object Day1 {

  def main(args: Array[String]): Unit = {
    val input = utils.loadInputAsStrings("2023/day1.txt")

    println(s"Part 1: ${calibrationValuesSum(input)}")
    println(s"Part 2: ${calibrationStringValuesSum(input)}")
  }

  private def calibrationValuesSum(input: Seq[String]): Int = {
    def extractValue(s: String) =
      for {
        first <- s.find(_.isDigit)
        last  <- s.findLast(_.isDigit)
      } yield s"$first$last".toInt

    input.flatMap(extractValue).sum
  }

  private def calibrationStringValuesSum(input: Seq[String]): Int = {
    val digits =
      Seq("one", "two", "three", "four", "five", "six", "seven", "eight", "nine").zip(1 to 9).toMap ++
        (1 to 9).map(_.toString).zip(1 to 9).toMap

    def extractValue(s: String) =
      for {
        first <- digits.keys.minByOption(d => if (s.indexOf(d) >= 0) s.indexOf(d) else Int.MaxValue)
        last  <- digits.keys.maxByOption(s.lastIndexOf)
      } yield s"${digits(first)}${digits(last)}".toInt

    input.flatMap(extractValue).sum
  }

}
