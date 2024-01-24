package com.github.cosminci.aoc._2017

import scala.util.chaining._

object Day15 {

  def main(args: Array[String]): Unit = {
    val (seedA, seedB) = (618, 814)

    println(s"Part 1: ${matchCount(seedA, seedB, iters = 40_000_000)}")
    println(s"Part 2: ${matchCount(seedA, seedB, iters = 5_000_000, divisorA = 4, divisorB = 8)}")
  }

  def matchCount(seedA: Long, seedB: Long, iters: Int, divisorA: Int = 1, divisorB: Int = 1): Int =
    (0 until iters).foldLeft(seedA, seedB, 0) { case ((a, b, cnt), _) =>
      val (nextA, nextB) = (next(a, factor = 16807, divisorA), next(b, factor = 48271, divisorB))
      val isMatch        = (nextA & 0xffff) == (nextB & 0xffff)
      (nextA, nextB, cnt + (if (isMatch) 1 else 0))
    }.pipe { case (_, _, cnt) => cnt }

  private def next(curr: Long, factor: Int, divisor: Int) =
    Iterator.iterate(curr)(_ * factor % 2147483647).drop(1).dropWhile(_ % divisor != 0).next()

}
