package com.github.cosminci.aoc._2017

import com.github.cosminci.aoc.utils

object Day2 {

  def main(args: Array[String]): Unit = {
    val spreadsheet = parseSpreadsheet(utils.loadInputAsStrings("2017/day2.txt"))

    println(s"Part 1: ${checksum(spreadsheet)}")
    println(s"Part 2: ${divisionSum(spreadsheet)}")
  }

  def checksum(spreadsheet: Seq[Seq[Int]]): Int =
    spreadsheet.map(row => row.max - row.min).sum

  def divisionSum(spreadsheet: Seq[Seq[Int]]): Int =
    spreadsheet.flatMap(divisionResult).sum

  private def divisionResult(row: Seq[Int]): Option[Int] =
    row.sorted.combinations(2).collectFirst { case Seq(a, b) if b % a == 0 => b / a }

  private def parseSpreadsheet(input: Seq[String]) =
    input.map(_.split('\t').toSeq.map(_.toInt))

}
