package com.github.cosminci.aoc._2021

import com.github.cosminci.aoc.utils

import scala.collection.mutable

object Day9 {
  def main(args: Array[String]): Unit = {
    val input = utils.loadInputAsStrings("2021/day9.txt").map(_.toCharArray.map(_ - '0').toSeq)

    println(s"Part I: ${riskLevel(input)}")
    println(s"Part II: ${largestBasinsProduct(input)}")
  }

  def riskLevel(grid: Seq[Seq[Int]]): Int = {
    val (n, m) = (grid.length - 1, grid.head.length - 1)
    val lowPoints = for {
      r <- grid.indices
      c <- grid(r).indices
      if utils.neighbours(n, m, r, c).forall { case (r1, c1) => grid(r)(c) < grid(r1)(c1) }
    } yield grid(r)(c) + 1

    lowPoints.sum
  }

  def largestBasinsProduct(grid: Seq[Seq[Int]]): Int = {
    val (n, m) = (grid.length - 1, grid.head.length - 1)
    val visited = mutable.Set.empty[(Int, Int)]

    def dfs(r: Int, c: Int): Int =
      1 + utils
        .neighbours(n, m, r, c)
        .collect {
          case (r1, c1) if grid(r1)(c1) != 9 && !visited.contains((r1, c1)) =>
            visited.add((r1, c1))
            dfs(r1, c1)
        }.sum

    val sizes = for {
      r <- grid.indices
      c <- grid(r).indices
      if grid(r)(c) != 9 && !visited.contains((r, c))
    } yield {
      visited.add((r, c))
      dfs(r, c)
    }

    sizes.sorted.takeRight(3).product
  }
}
