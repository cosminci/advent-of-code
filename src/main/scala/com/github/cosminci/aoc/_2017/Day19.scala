package com.github.cosminci.aoc._2017

import com.github.cosminci.aoc.utils

object Day19 {

  def main(args: Array[String]): Unit = {
    val grid = utils.loadInputAsStrings("2017/day19.txt")

    println(s"Part 1: ${lettersAfterTraversal(grid)}")
    println(s"Part 2: ${stepsToTraverse(grid)}")
  }

  def lettersAfterTraversal(grid: Seq[String]): Seq[Char] =
    traverse(grid).collect { case Pos(r, c) if grid(r)(c).isLetter => grid(r)(c) }.mkString

  def stepsToTraverse(grid: Seq[String]): Int =
    traverse(grid).size

  final case class Pos(r: Int, c: Int) {
    def +(d: Dir): Pos = Pos(r + d.dr, c + d.dc)
  }

  final case class Dir(dr: Int, dc: Int) {
    def orthogonal(other: Dir): Boolean = dr * other.dr + dc * other.dc == 0
  }

  private val dirs = Seq(Dir(0, 1), Dir(0, -1), Dir(1, 0), Dir(-1, 0))

  private def traverse(grid: Seq[String]) = {
    def valid(p: Pos) = p.r.min(p.c) >= 0 && p.r.max(p.c) < grid.length && grid(p.r)(p.c) != ' '

    @annotation.tailrec
    def dfs(pos: Pos, dir: Dir, path: Seq[Pos]): Seq[Pos] =
      grid(pos.r)(pos.c) match {
        case ' '       => path
        case '|' | '-' => dfs(pos + dir, dir, path :+ pos)
        case '+' =>
          val newDir = dirs.find(newDir => dir.orthogonal(newDir) && valid(pos + newDir)).get
          dfs(pos + newDir, newDir, path :+ pos)
        case _ => dfs(pos + dir, dir, path :+ pos)
      }

    dfs(Pos(0, grid.head.indexOf('|')), Dir(1, 0), path = Seq.empty)
  }

}
