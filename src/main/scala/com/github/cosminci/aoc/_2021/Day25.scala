package com.github.cosminci.aoc._2021

import com.github.cosminci.aoc.utils

import scala.annotation.tailrec

object Day25 {
  def main(args: Array[String]): Unit =
    println(s"Part I: ${stepsUntilStable(utils.loadInputAsStrings("2021/day25.txt"))}")

  def stepsUntilStable(map: Seq[String]): Int = {
    val (m, n) = (map.length, map.head.length)

    @tailrec
    def dfs(eastFacing: Set[(Int, Int)], southFacing: Set[(Int, Int)], steps: Int): Int = {
      val (newEastFacing, newSouthFacing) = updatePositions(eastFacing, southFacing, m, n)
      if (newEastFacing == eastFacing && newSouthFacing == southFacing) steps + 1
      else dfs(newEastFacing, newSouthFacing, steps + 1)
    }

    val (eastFacing, southFacing) = initialPositions(map, m, n)
    dfs(eastFacing, southFacing, steps = 0)
  }

  private def initialPositions(map: Seq[String], m: Int, n: Int) = {
    val positions = for {
      r <- 0 until m
      c <- 0 until n
      if map(r)(c) != '.'
    } yield (r, c, map(r)(c))

    val eastFacing  = positions.collect { case (r, c, '>') => (r, c) }.toSet
    val southFacing = positions.collect { case (r, c, 'v') => (r, c) }.toSet

    (eastFacing, southFacing)
  }

  private def updatePositions(eastFacing: Set[(Int, Int)], southFacing: Set[(Int, Int)], m: Int, n: Int) = {
    val newEastFacing = eastFacing.map { case currPos @ (r, c) =>
      val nextPos = (r, (c + 1) % n)
      if (Seq(eastFacing, southFacing).exists(_.contains(nextPos))) currPos else nextPos
    }
    val newSouthFacing = southFacing.map { case currPos @ (r, c) =>
      val nextPos = ((r + 1) % m, c)
      if (Seq(southFacing, newEastFacing).exists(_.contains(nextPos))) currPos else nextPos
    }
    (newEastFacing, newSouthFacing)
  }
}
