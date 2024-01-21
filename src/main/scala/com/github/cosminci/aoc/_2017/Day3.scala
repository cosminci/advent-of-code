package com.github.cosminci.aoc._2017

object Day3 {

  def main(args: Array[String]): Unit = {
    val input = 347991

    println(s"Part 1: ${stepsFromCenter(address = input)}")
    println(s"Part 2: ${firstValueHigherThan(value = input)}")
  }

  def stepsFromCenter(address: Int): Int = {
    @annotation.tailrec
    def dfs(ring: Int, maxAddress: Int): (Int, Int) =
      if (address <= maxAddress) (ring, address - (maxAddress - ring * 8))
      else dfs(ring + 1, maxAddress + (ring + 1) * 8)

    val (ring, offset) = dfs(ring = 0, maxAddress = 1)
    ring + (offset % (ring * 2) - ring).abs
  }

  case class Pos(x: Int, y: Int)
  private val dirs = Seq(Pos(1, 0), Pos(0, 1), Pos(-1, 0), Pos(0, -1))

  def firstValueHigherThan(value: Int): Int = {
    @annotation.tailrec
    def dfs(grid: Map[Pos, Int], pos: Pos, dir: Pos, sum: Int): Int =
      if (sum > value) sum
      else {
        val (nextPos, nextDir) = decideNext(grid, pos, dir)
        val nextSum            = nei(nextPos).flatMap(grid.get).sum
        dfs(grid.updated(nextPos, nextSum), nextPos, nextDir, nextSum)
      }

    dfs(grid = Map(Pos(0, 0) -> 1), pos = Pos(0, 0), dir = Pos(0, -1), sum = 1)
  }

  private def decideNext(grid: Map[Pos, Int], curr: Pos, dir: Pos) = {
    val next = Pos(curr.x + dir.x, curr.y + dir.y)
    if (nei(next).flatMap(grid.get).size > 1) (next, dir)
    else {
      val newDir = dirs((dirs.indexOf(dir) + 1) % dirs.length)
      (Pos(curr.x + newDir.x, curr.y + newDir.y), newDir)
    }
  }

  private def nei(p: Pos) =
    Seq((1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1), (0, -1), (1, -1))
      .map { case (dx, dy) => Pos(p.x + dx, p.y + dy) }

}
