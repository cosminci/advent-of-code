package com.github.cosminci.aoc._2024

import com.github.cosminci.aoc.utils

object Day8 {

  def main(args: Array[String]): Unit = {
    val grid = utils.loadInputAsStrings("2024/day8.txt")

    println(s"Part 1: ${antinodeCount(grid, findFirstAntinodes(grid))}")
    println(s"Part 2: ${antinodeCount(grid, findAllAntinodes(grid))}")
  }

  final case class Pos(r: Int, c: Int)
  final case class Node(freq: Char, pos: Pos)

  private def antinodeCount(grid: Seq[String], findAntinodesFn: Seq[Pos] => Iterator[Pos]) =
    findNodes(grid).values.flatMap(findAntinodesFn).toSet.size

  private def findFirstAntinodes(grid: Seq[String])(nodes: Seq[Pos]) =
    nodes.combinations(2).flatMap { case Seq(p1, p2) =>
      val (dr, dc) = (p1.r - p2.r, p1.c - p2.c)
      Seq(Pos(p1.r + dr, p1.c + dc), Pos(p2.r - dr, p2.c - dc)).filter(withinBounds(_, grid))
    }

  private def findAllAntinodes(grid: Seq[String])(nodes: Seq[Pos]) =
    nodes.combinations(2).flatMap { case Seq(p1, p2) =>
      val (dr, dc) = (p1.r - p2.r, p1.c - p2.c)
      Iterator.iterate(p1) { case Pos(r, c) => Pos(r + dr, c + dc) }.takeWhile(withinBounds(_, grid)) ++
        Iterator.iterate(p2) { case Pos(r, c) => Pos(r - dr, c - dc) }.takeWhile(withinBounds(_, grid))
    }

  private def withinBounds(p: Pos, grid: Seq[String]) = p.r.min(p.c) >= 0 && p.r.max(p.c) < grid.length

  private def findNodes(grid: Seq[String]) = {
    val nodes = for {
      r <- grid.indices
      c <- grid(r).indices
      if grid(r)(c) != '.'
    } yield Node(grid(r)(c), Pos(r, c))

    nodes.groupMap(_.freq)(_.pos)
  }

}
