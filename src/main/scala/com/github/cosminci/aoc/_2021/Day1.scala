package com.github.cosminci.aoc._2021

import com.github.cosminci.aoc.utils

object Day1 {
  def main(args: Array[String]): Unit = {
    val measurements = utils.loadInputAsInts("2021/day1.txt")
    println(s"Part I: ${countDepthIncreases(measurements, step = 1)}")
    println(s"Part II: ${countDepthIncreases(measurements, step = 3)}")
  }

  def countDepthIncreases(measurements: Seq[Int], step: Int): Int =
    (step until measurements.length).count(i => measurements(i) > measurements(i - step))
}
