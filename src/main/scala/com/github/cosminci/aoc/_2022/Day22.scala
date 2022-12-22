package com.github.cosminci.aoc._2022

import cats.parse.{Numbers, Parser => P}
import com.github.cosminci.aoc.utils

import scala.util.Try
import scala.util.chaining._

object Day22 {

  def main(args: Array[String]): Unit = {
    val grid  = utils.loadInputAsStrings("2022/day22_map.txt")
    val moves = movesParser.parseAll(utils.loadInputAsStrings("2022/day22_moves.txt").head).toOption.get

    println(s"Part 1: ${finalPassword(grid, moves, wrap2D)}")
    println(s"Part 2: ${finalPassword(grid, moves, wrap3D)}")
  }

  type Direction = (Int, Int)
  private val directions = Seq((0, 1), (1, 0), (0, -1), (-1, 0))

  case class Move(turn: Int, distance: Int)
  case class Pos(r: Int, c: Int, dir: Int)

  def finalPassword(grid: Seq[String], moves: Seq[Move], wrap: (Seq[String], Pos) => Pos): Int =
    moves
      .foldLeft(Pos(0, grid.head.indexWhere(_ != ' '), 0))(nextPosition(grid, wrap))
      .pipe { case Pos(x, y, dirIdx) => (x + 1) * 1000 + 4 * (y + 1) + dirIdx }

  private def nextPosition(grid: Seq[String], wrap: (Seq[String], Pos) => Pos)(prev: Pos, move: Move): Pos = {
    val curr = prev.copy(dir = math.floorMod(prev.dir + move.turn, directions.length))
    Iterator
      .iterate(curr) { case curr @ Pos(r, c, dir) =>
        val (dr, dc) = directions(dir)
        Try(grid(r + dr)(c + dc)).getOrElse(' ') match {
          case '.' => Pos(r + dr, c + dc, dir)
          case '#' => return curr
          case ' ' =>
            val next @ Pos(nr, nc, _) = wrap(grid, curr)
            if (grid(nr)(nc) == '#') return curr else next
        }
      }
      .drop(move.distance)
      .next()
  }

  private def wrap2D(grid: Seq[String], pos: Pos) = {
    val (dr, dc) = directions(pos.dir)
    Iterator
      .iterate((pos.r, pos.c)) { case (r, c) => (r - dr, c - dc) }
      .dropWhile { case (r, c) => Try(grid(r - dr)(c - dc)).toOption.exists(_ != ' ') }
      .next()
      .pipe { case (r, c) => Pos(r, c, pos.dir) }
  }

  private def wrap3D(ignored: Seq[String], pos: Pos) = {
    val (x, y) = (pos.r / 50, pos.c / 50)
    val (nx, ny, ndir) = (x, y, pos.dir) match {
      case (0, 1, 3) => (3, 0, 0)
      case (0, 1, 2) => (2, 0, 0)
      case (0, 2, 3) => (3, 0, 3)
      case (0, 2, 0) => (2, 1, 2)
      case (0, 2, 1) => (1, 1, 2)
      case (1, 1, 0) => (0, 2, 3)
      case (1, 1, 2) => (2, 0, 1)
      case (2, 0, 3) => (1, 1, 0)
      case (2, 0, 2) => (0, 1, 0)
      case (2, 1, 0) => (0, 2, 2)
      case (2, 1, 1) => (3, 0, 2)
      case (3, 0, 0) => (2, 1, 3)
      case (3, 0, 1) => (0, 2, 1)
      case (3, 0, 2) => (0, 1, 1)
    }
    val (rOffset, cOffset)   = (pos.r % 50, pos.c % 50)
    val nOffset              = Seq(rOffset, 49 - cOffset, 49 - rOffset, cOffset)(pos.dir)
    val (nrOffset, ncOffset) = Seq((nOffset, 0), (0, 49 - nOffset), (49 - nOffset, 49), (49, nOffset))(ndir)
    val (nr, nc)             = (nx * 50 + nrOffset, ny * 50 + ncOffset)
    Pos(nr, nc, ndir)
  }

  private val numParser       = Numbers.digits.map(_.toInt)
  private val directionParser = P.fromCharMap(Map('R' -> 1, 'L' -> -1)).orElse(P.pure(0)).with1
  private val movesParser     = P.repSep0((directionParser ~ numParser).map(Move.tupled), P.unit)

}
