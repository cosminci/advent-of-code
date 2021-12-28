package com.github.cosminci.aoc._2020

import com.github.cosminci.aoc.utils

object Day2 {

  def main(args: Array[String]): Unit = {
    val inputData = utils.loadInputAsStrings("2020/day2.txt").map { str =>
      str.split(" ").toSeq match {
        case Seq(range, charStr, pw) =>
          val Array(low, high) = range.split("-").map(_.toInt)
          Rule(low, high, charStr.head, pw)
      }
    }

    println(s"Part 1: ${countValidRangePasswords(inputData)}")
    println(s"Part 2: ${countValidPositionPasswords(inputData)}")
  }

  case class Rule(n1: Int, n2: Int, char: Char, pw: String)

  private def countValidRangePasswords(inputData: Seq[Rule]): Int =
    inputData.count { case Rule(minCount, maxCount, char, pw) =>
      val count = pw.count(_ == char)
      count >= minCount && count <= maxCount
    }

  private def countValidPositionPasswords(inputData: Seq[Rule]): Int =
    inputData.count { case Rule(position1, position2, char, pw) =>
      (pw.charAt(position1 - 1) == char) ^ (pw.charAt(position2 - 1) == char)
    }

}
