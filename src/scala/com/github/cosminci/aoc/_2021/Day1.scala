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

  def countWindowDepthIncreases(measurements: Seq[Int]): Int =
    (3 until measurements.length).count(i => measurements(i) > measurements(i - 3))
}
