package com.github.cosminci.aoc._2023

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.syntax.parallel._
import com.github.cosminci.aoc._2023.Day16.Dir.Dir
import com.github.cosminci.aoc.utils
import monocle.syntax.all._

object Day16 {

  // Requires increased JVM stack size e.g. `-Xss10m`
  def main(args: Array[String]): Unit = {
    val grid = utils.loadInputAsStrings("2023/day16.txt")

    println(s"Part 1: ${energizedTiles(startMove = Move(Pos(0, 0), Dir.E), grid)}")
    println(s"Part 2: ${maxEnergizedTiles(grid)}")
  }

  final case class Pos(r: Int, c: Int)
  object Dir extends Enumeration {
    type Dir = Value
    val E, W, N, S = Value
  }
  final case class Move(p: Pos, d: Dir)

  def energizedTiles(startMove: Move, grid: Seq[String]): Int = {
    def dfs(move: Move, visited: Set[Move]): Set[Move] = {
      if (!isValid(move.p, grid) || visited.contains(move)) visited
      else nextDir(move, grid).foldLeft(visited + move)((visited, d) => dfs(nextMove(Move(move.p, d)), visited))
    }
    dfs(startMove, visited = Set.empty).map(_.p).size
  }

  def maxEnergizedTiles(grid: Seq[String]): Int = {
    val (m, n)         = (grid.length, grid.head.length)
    val horizontal     = (1 until m - 1).flatMap { r => Seq(Move(Pos(r, 0), Dir.E), Move(Pos(r, n - 1), Dir.W)) }
    val vertical       = (1 until n - 1).flatMap { c => Seq(Move(Pos(0, c), Dir.S), Move(Pos(m - 1, c), Dir.N)) }
    val topLeftCorner  = Seq(Move(Pos(0, 0), Dir.E), Move(Pos(0, 0), Dir.S))
    val topRightCorner = Seq(Move(Pos(0, n - 1), Dir.W), Move(Pos(0, n - 1), Dir.S))
    val btmLeftCorner  = Seq(Move(Pos(m - 1, 0), Dir.E), Move(Pos(m - 1, 0), Dir.N))
    val btmRightCorner = Seq(Move(Pos(m - 1, n - 1), Dir.W), Move(Pos(m - 1, n - 1), Dir.N))
    val starts         = vertical ++ horizontal ++ topLeftCorner ++ topRightCorner ++ btmLeftCorner ++ btmRightCorner

    starts.toList.parTraverse(start => IO(energizedTiles(start, grid))).unsafeRunSync()(IORuntime.global).max
  }

  private def isValid(pos: Pos, grid: Seq[String]) =
    pos.r >= 0 && pos.r < grid.length && pos.c >= 0 && pos.c < grid(pos.r).length

  private def nextMove(move: Move) = move.d match {
    case Dir.E => move.focus(_.p.c).modify(_ + 1)
    case Dir.W => move.focus(_.p.c).modify(_ - 1)
    case Dir.N => move.focus(_.p.r).modify(_ - 1)
    case Dir.S => move.focus(_.p.r).modify(_ + 1)
  }

  private def nextDir(move: Move, grid: Seq[String]) = (move.d, grid(move.p.r)(move.p.c)) match {
    case (Dir.E, '/')  => Seq(Dir.N)
    case (Dir.E, '\\') => Seq(Dir.S)
    case (Dir.W, '/')  => Seq(Dir.S)
    case (Dir.W, '\\') => Seq(Dir.N)
    case (Dir.N, '/')  => Seq(Dir.E)
    case (Dir.N, '\\') => Seq(Dir.W)
    case (Dir.S, '/')  => Seq(Dir.W)
    case (Dir.S, '\\') => Seq(Dir.E)
    case (Dir.E, '|')  => Seq(Dir.N, Dir.S)
    case (Dir.W, '|')  => Seq(Dir.N, Dir.S)
    case (Dir.N, '-')  => Seq(Dir.E, Dir.W)
    case (Dir.S, '-')  => Seq(Dir.E, Dir.W)
    case _             => Seq(move.d)
  }

}
