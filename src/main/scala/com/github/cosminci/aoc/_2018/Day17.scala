package com.github.cosminci.aoc._2018

import com.github.cosminci.aoc.utils

import scala.util.chaining._

object Day17 {

  def main(args: Array[String]): Unit = {
    val tiles = parseInput(utils.loadInputAsStrings("2018/day17.txt"))

    println(s"Part 1: ${countWaterTouchedTiles(tiles)}")
    println(s"Part 2: ${countWaterReservoirTiles(tiles)}")
  }

  final case class Pos(x: Int, y: Int)

  sealed trait TileState
  case object Clay    extends TileState
  sealed trait Water  extends TileState
  case object Settled extends Water
  case object Flowing extends Water

  sealed trait Direction
  case object Down  extends Direction
  case object Left  extends Direction
  case object Right extends Direction

  def countWaterTouchedTiles(initialTiles: Map[Pos, TileState]): Int = {
    val (minY, maxY) = (initialTiles.keys.map(_.y).min, initialTiles.keys.map(_.y).max)
    val tiles        = tilesAfterWaterSettles(initialTiles, maxY)
    tiles.collect { case (pos, _: Water) if pos.y >= minY && pos.y <= maxY => 1 }.sum
  }

  def countWaterReservoirTiles(initialTiles: Map[Pos, TileState]): Int = {
    val (minY, maxY) = (initialTiles.keys.map(_.y).min, initialTiles.keys.map(_.y).max)
    val tiles        = tilesAfterWaterSettles(initialTiles, maxY)
    tiles.collect { case (pos, Settled) if pos.y >= minY && pos.y <= maxY => 1 }.sum
  }

  private def tilesAfterWaterSettles(initialTiles: Map[Pos, TileState], maxY: Int) = {
    def dfs(pos: Pos, tiles: Map[Pos, TileState], dir: Direction): Map[Pos, TileState] =
      if (pos.y > maxY) tiles.updated(pos, Flowing)
      else if (tiles.contains(pos)) tiles
      else {
        val (leftPos, rightPos, downPos) = (Pos(pos.x - 1, pos.y), Pos(pos.x + 1, pos.y), Pos(pos.x, pos.y + 1))
        val tilesAfterDown = dfs(downPos, tiles.updated(pos, Flowing), Down)
        if (tilesAfterDown(downPos) == Flowing) tilesAfterDown
        else dir match {
          case Left  => dfs(leftPos, tilesAfterDown, Left)
          case Right => dfs(rightPos, tilesAfterDown, Right)
          case Down =>
            val tilesAfterLeft  = dfs(leftPos, tilesAfterDown, Left)
            val tilesAfterRight = dfs(rightPos, tilesAfterLeft, Right)
            if (isBounded(pos, tilesAfterRight)) settleRow(pos, tilesAfterRight) else tilesAfterRight
        }
      }
    dfs(Pos(x = 500, y = 0), initialTiles, dir = Down)
  }

  private def settleRow(pos: Pos, tiles: Map[Pos, TileState]) =
    (rowEnd(pos, dx = -1, tiles).pipe(_.x + 1) until rowEnd(pos, dx = 1, tiles).x)
      .foldLeft(tiles)((tiles, x) => tiles.updated(Pos(x, pos.y), Settled))

  private def isBounded(pos: Pos, tiles: Map[Pos, TileState]) =
    tiles.get(rowEnd(pos, dx = -1, tiles)).contains(Clay) &&
      tiles.get(rowEnd(pos, dx = 1, tiles)).contains(Clay)

  private def rowEnd(pos: Pos, dx: Int, tiles: Map[Pos, TileState]) =
    Iterator
      .iterate(pos) { case Pos(x, y) => Pos(x + dx, y) }
      .dropWhile(tiles.get(_).contains(Flowing))
      .next()

  private def parseInput(input: Seq[String]) =
    input.flatMap {
      case s"x=$v1, y=$v2..$v3" => (v2.toInt to v3.toInt).map(y => Pos(v1.toInt, y))
      case s"y=$v1, x=$v2..$v3" => (v2.toInt to v3.toInt).map(x => Pos(x, v1.toInt))
    }.map(_ -> Clay).toMap

}
