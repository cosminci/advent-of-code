package com.github.cosminci.aoc._2021

import com.github.cosminci.aoc.utils

object Day1 {
  def main(args: Array[String]): Unit = {
    val measurements = utils.loadInputAsInts("2021/day1.txt")
    println(countDepthIncreases(measurements))
    println(countWindowDepthIncreases(measurements))
  }

  def countDepthIncreases(measurements: Seq[Int]): Int =
    (1 until measurements.length).count(i => measurements(i) > measurements(i - 1))

  def countWindowDepthIncreases(measurements: Seq[Int]): Int = {
    val windowSums = measurements.sliding(3).map(_.sum).toSeq
    (1 until windowSums.length).count(i => windowSums(i) > windowSums(i - 1))
  }
}
