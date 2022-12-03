package com.github.cosminci.aoc._2022

import com.github.cosminci.aoc.utils

import scala.util.chaining._

object Day3 {

  def main(args: Array[String]): Unit = {
    val input = utils.loadInputAsStrings("2022/day3.txt")

    println(s"Part 1: ${prioritySumPerElf(input)}")
    println(s"Part 2: ${prioritySumPerGroup(input)}")
  }

  def prioritySumPerElf(input: Seq[String]): Int =
    input.map { items =>
      items
        .splitAt(items.length / 2)
        .pipe { case (fh, sh) => fh.intersect(sh).head }
        .pipe(priority)
    }.sum

  def prioritySumPerGroup(input: Seq[String]): Int =
    input
      .grouped(3)
      .map(group => group.reduce(_ intersect _).head.pipe(priority))
      .sum

  private def priority(item: Char) =
    if (item.isLower) item - 'a' + 1
    else item - 'A' + 27

}
