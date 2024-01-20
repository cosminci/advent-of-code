package com.github.cosminci.aoc._2017

import com.github.cosminci.aoc.utils

object Day1 {

  def main(args: Array[String]): Unit = {
    val input = utils.loadInputAsStrings("2017/day1.txt").head

    println(s"Part 1: ${solveCaptcha(input, step = 1)}")
    println(s"Part 2: ${solveCaptcha(input, step = input.length / 2)}")
  }

  def solveCaptcha(s: String, step: Int): Int =
    s.indices.collect { case i if s(i) == s((i + step) % s.length) => s(i) - '0' }.sum

}
