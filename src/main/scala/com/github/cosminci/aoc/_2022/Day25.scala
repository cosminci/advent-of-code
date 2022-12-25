package com.github.cosminci.aoc._2022

import com.github.cosminci.aoc.utils

import scala.math.Integral.Implicits._
import scala.util.chaining._

object Day25 {

  def main(args: Array[String]): Unit = {
    val input = utils.loadInputAsStrings("2022/day25.txt")

    println(s"Part 1: ${snafuTotalFuelNumber(input)}")
  }

  def snafuTotalFuelNumber(snafuFuelNumbers: Seq[String]): String =
    snafuFuelNumbers
      .map(snafuToDecimal)
      .sum
      .pipe(decimalToSnafu)

  private val snafuValues = Map('-' -> -1, '=' -> -2).withDefault(_ - '0')
  private def snafuToDecimal(v: String) =
    v.reverse.zipWithIndex.map { case (char, fifth) => snafuValues(char) * math.pow(5, fifth).toLong }.sum

  private def decimalToSnafu(v: Long): String =
    if (v == 0) ""
    else v /% 5 match {
      case (quot, 0) => decimalToSnafu(quot) ++ "0"
      case (quot, 1) => decimalToSnafu(quot) ++ "1"
      case (quot, 2) => decimalToSnafu(quot) ++ "2"
      case (quot, 3) => decimalToSnafu(quot + 1) ++ "="
      case (quot, _) => decimalToSnafu(quot + 1) ++ "-"
    }

}
