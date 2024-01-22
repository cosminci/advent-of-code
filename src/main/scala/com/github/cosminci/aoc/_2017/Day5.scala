package com.github.cosminci.aoc._2017

import com.github.cosminci.aoc.utils

object Day5 {

  def main(args: Array[String]): Unit = {
    val instructions = utils.loadInputAsInts("2017/day5.txt")

    println(s"Part 1: ${stepsToExit(instructions, dxFn = _ => 1)}")
    println(s"Part 2: ${stepsToExit(instructions, dxFn = i => if (i >= 3) -1 else 1)}")
  }

  def stepsToExit(instructions: Seq[Int], dxFn: Int => Int): Int = {
    @annotation.tailrec
    def dfs(instructions: Map[Int, Int], i: Int, steps: Int): Int =
      instructions.get(i) match {
        case None => steps
        case Some(offset) =>
          dfs(instructions.updated(i, instructions(i) + dxFn(offset)), i + offset, steps + 1)
      }
    dfs(instructions.indices.zip(instructions).toMap, i = 0, steps = 0)
  }

}
