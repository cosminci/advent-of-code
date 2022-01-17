package com.github.cosminci.aoc._2021

import com.github.cosminci.aoc.utils

object Day9 {
  def main(args: Array[String]): Unit = {
    val input = utils.loadInputAsStrings("2021/day9.txt").map(_.toCharArray.map(_ - '0').toSeq)

    println(s"Part I: ${riskLevel(input)}")
    println(s"Part II: ${largestBasinsProduct(input)}")
  }

  def riskLevel(grid: Seq[Seq[Int]]): Int = {
    val (m, n) = (grid.length - 1, grid.head.length - 1)
    val lowPoints = for {
      r <- grid.indices
      c <- grid(r).indices
      if utils.neighbours(m, n, r, c).forall { case (r1, c1) => grid(r)(c) < grid(r1)(c1) }
    } yield grid(r)(c) + 1

    lowPoints.sum
  }

  def largestBasinsProduct(grid: Seq[Seq[Int]]): Int = {
    val (m, n) = (grid.length - 1, grid.head.length - 1)

    def dfs(r: Int, c: Int, visited: Set[(Int, Int)]): (Set[(Int, Int)], Int) =
      utils.neighbours(m, n, r, c).foldLeft(visited, 1) { case ((visited, size), (r, c)) =>
        if (grid(r)(c) == 9 || visited.contains((r, c))) (visited, size)
        else {
          val (newVisited, count) = dfs(r, c, visited + ((r, c)))
          (newVisited, size + count)
        }
      }

    (0 until n).flatMap(r => (0 until m).map(c => (r, c)))
      .foldLeft(Set.empty[(Int, Int)], Seq.empty[Int]) { case ((visited, sizes), (r, c)) =>
        if (grid(r)(c) == 9 || visited.contains((r, c))) (visited, sizes)
        else {
          val (newVisited, size) = dfs(r, c, visited + ((r, c)))
          (newVisited, sizes :+ size)
        }
      }._2.sorted.takeRight(3).product
  }
}
