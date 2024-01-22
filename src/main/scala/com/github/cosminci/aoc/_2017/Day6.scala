package com.github.cosminci.aoc._2017

import com.github.cosminci.aoc.utils

object Day6 {

  def main(args: Array[String]): Unit = {
    val memoryBanks = parseInput(utils.loadInputAsStrings("2017/day6.txt").head)

    println(s"Part 1: ${cyclesUntilRepeat(memoryBanks)}")
    println(s"Part 2: ${repeatLoopSize(memoryBanks)}")
  }

  def cyclesUntilRepeat(memoryBanks: Map[Int, Int]): Int = {
    @annotation.tailrec
    def dfs(mem: Set[Map[Int, Int]], curr: Map[Int, Int]): Int =
      if (mem.contains(curr)) mem.size
      else dfs(mem + curr, redistribute(curr))

    dfs(mem = Set.empty, curr = memoryBanks)
  }

  def repeatLoopSize(memoryBanks: Map[Int, Int]): Int = {
    @annotation.tailrec
    def dfs(mem: Map[Map[Int, Int], Int], curr: Map[Int, Int]): Int =
      mem.get(curr) match {
        case Some(i) => mem.size - i
        case None    => dfs(mem.updated(curr, mem.size), redistribute(curr))
      }

    dfs(mem = Map.empty, curr = memoryBanks)
  }

  private def redistribute(memoryBanks: Map[Int, Int]): Map[Int, Int] = {
    @annotation.tailrec
    def dfs(memoryBanks: Map[Int, Int], i: Int, blocks: Int): Map[Int, Int] =
      if (blocks == 0) memoryBanks
      else dfs(memoryBanks.updated(i, memoryBanks(i) + 1), (i + 1) % memoryBanks.size, blocks - 1)

    val (i, blocks) = memoryBanks.maxBy { case (i, blocks) => (blocks, -i) }
    dfs(memoryBanks.updated(i, 0), (i + 1) % memoryBanks.size, blocks)
  }

  private def parseInput(input: String) = {
    val memoryBanks = input.split('\t').map(_.toInt)
    memoryBanks.indices.zip(memoryBanks).toMap
  }

}
