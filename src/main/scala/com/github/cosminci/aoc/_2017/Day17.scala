package com.github.cosminci.aoc._2017

object Day17 {

  def main(args: Array[String]): Unit = {
    val steps = 382

    println(s"Part 1: ${valueAfter2017(steps)}")
    println(s"Part 2: ${valueAfter0(steps)}")
  }

  private def valueAfter2017(steps: Int): Int = {
    @annotation.tailrec
    def dfs(iter: Int, pos: Int, buffer: Seq[Int]): Int =
      if (iter > 2017) buffer((buffer.indexOf(2017) + 1) % buffer.size)
      else {
        val newPos = (pos + steps) % iter + 1
        dfs(iter + 1, newPos, buffer.patch(newPos, Seq(iter), 0))
      }

    dfs(iter = 1, pos = 0, buffer = Seq(0))
  }

  private def valueAfter0(steps: Int): Int = {
    @annotation.tailrec
    def dfs(iter: Int, pos: Int, res: Int): Int =
      if (iter > 50_000_000) res
      else {
        val newPos = (pos + steps) % iter + 1
        dfs(iter + 1, newPos, if (newPos == 1) iter else res)
      }

    dfs(iter = 1, pos = 0, res = -1)
  }

}
