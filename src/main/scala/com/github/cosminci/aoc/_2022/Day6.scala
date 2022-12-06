package com.github.cosminci.aoc._2022

import com.github.cosminci.aoc.utils

import scala.util.chaining._

object Day6 {

  def main(args: Array[String]): Unit = {
    val dataStream = utils.loadInputAsStrings("2022/day6.txt").head

    println(s"Part 1: ${endOfWindowOffset(dataStream, windowSize = 4)}")
    println(s"Part 2: ${endOfWindowOffset(dataStream, windowSize = 14)}")
  }

  def endOfWindowOffset(dataStream: String, windowSize: Int): Int =
    dataStream
      .sliding(windowSize)
      .indexWhere(_.distinct.length == windowSize)
      .pipe(_ + windowSize)
}
