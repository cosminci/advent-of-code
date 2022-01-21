package com.github.cosminci.aoc._2020

import com.github.cosminci.aoc.utils

import scala.annotation.tailrec

object Day20 {

  def main(args: Array[String]): Unit = {
    val tileInput = utils.loadInputAsStrings("2020/day20.txt")
    val tiles     = parseInput(tileInput)
    val gridSize  = math.sqrt(tiles.size).toInt

    val monsterInput = utils.loadInputAsStrings("2020/day20_monster.txt")
    val monster      = parseMonster(monsterInput)

    val arrangements = validArrangements(gridSize, tiles)
    println(s"Part 1: ${cornerTileIdProduct(gridSize, arrangements.head.map(_.id))}")
    println(s"Part 2: ${waterRoughness(gridSize, tiles, arrangements, monster)}")
  }

  type Edges        = ((Int, Int, Int, Int), (Int, Int, Int, Int))
  type Tile         = (Seq[String], Edges)
  type Tiles        = Map[Int, Tile]
  type Monster      = Seq[(Int, Int)]
  type Arrangements = Seq[Seq[Placement]]
  case class Placement(id: Int, isMirror: Boolean, rotation: Int, edges: Edges)

  def cornerTileIdProduct(gridSize: Int, tileIds: Seq[Int]): Long =
    Seq((0, 0), (0, gridSize - 1), (gridSize - 1, 0), (gridSize - 1, gridSize - 1)).map { case (r, c) =>
      tileIds(r * gridSize + c).toLong
    }.product

  private def waterRoughness(gridSize: Int, tiles: Tiles, arrangements: Arrangements, monster: Monster): Int =
    arrangements.map { arrangement =>
      val grid = reconstructGrid(gridSize, tiles, arrangement)
      grid.map(_.count(_ == '#')).sum - countMonsters(monster, grid) * monster.length
    }.min

  def validArrangements(gridSize: Int, tiles: Tiles): Arrangements = {
    def dfs(available: Tiles, placed: Seq[Placement]): Arrangements =
      if (available.isEmpty) Seq(placed)
      else
        available.flatMap { case (id, (_, edges)) =>
          tileChoices.flatMap { case (isMirror, rotation) =>
            val edgesChoice = mirrorEdges(isMirror, rotateEdges(rotation, edges))
            if (!fitsPrevious(gridSize, placed, edgesChoice)) Seq.empty
            else dfs(available - id, placed :+ Placement(id, isMirror, rotation, edgesChoice))
          }
        }.toSeq

    dfs(available = tiles, placed = Seq.empty)
  }

  private val tileChoices = for {
    isMirror <- Seq(false, true)
    rotation <- 0 to 3
  } yield (isMirror, rotation)

  private def countMonsters(monster: Monster, grid: Seq[String]) = {
    val (m, n) = monster.foldLeft(0, 0) { case ((mx, my), (x, y)) =>
      (mx.max(x), my.max(y))
    }

    val indices = for {
      x <- 0 until grid.length - m
      y <- 0 until grid.head.length - n
    } yield (x, y)

    indices.count { case (x, y) =>
      monster.forall { case (mx, my) =>
        grid(x + mx)(y + my) == '#'
      }
    }
  }

  private def reconstructGrid(gridSize: Int, tiles: Tiles, arrangement: Seq[Placement]) =
    alignTiles(tiles, arrangement)
      .grouped(gridSize)
      .flatMap { group =>
        group.head.indices.map { row =>
          group.map(_(row)).mkString
        }
      }
      .toSeq

  private def alignTiles(tiles: Tiles, arrangement: Seq[Placement]) =
    arrangement.map { case Placement(id, isMirror, rotation, _) =>
      mirrorTile(isMirror, rotateTile(rotation, stripMargin(tiles(id)._1)))
    }

  private def stripMargin(grid: Seq[String]) =
    grid.tail.dropRight(1).map(_.tail.dropRight(1))

  @tailrec
  private def rotateTile(times: Int, tile: Seq[String]): Seq[String] =
    if (times == 0) tile
    else rotateTile(times - 1, mirrorTile(bool = true, tile.transpose.map(_.mkString)))

  private def mirrorTile(bool: Boolean, tile: Seq[String]) =
    if (!bool) tile else tile.map(_.reverse)

  private def fitsPrevious(gridSize: Int, placed: Seq[Placement], orientation: Edges) = {
    val fitsUp =
      if (placed.length < gridSize) true
      else isVerticalMatch(placed(placed.length - gridSize).edges, orientation)

    val fitsLeft =
      if (placed.length % gridSize == 0) true
      else isHorizontalMatch(placed.last.edges, orientation)

    fitsUp && fitsLeft
  }

  @tailrec
  private def rotateEdges(times: Int, edges: Edges): Edges =
    if (times == 0) edges
    else
      edges match {
        case ((e1, e2, e3, e4), (re1, re2, re3, re4)) =>
          rotateEdges(times - 1, ((e4, e1, e2, e3), (re4, re1, re2, re3)))
      }

  private def mirrorEdges(bool: Boolean, edges: Edges) =
    if (!bool) edges
    else
      edges match {
        case ((e1, e2, e3, e4), (re1, re2, re3, re4)) => ((re1, re4, re3, re2), (e1, e4, e3, e2))
      }

  private def isVerticalMatch(top: Edges, bottom: Edges) = (top, bottom) match {
    case (((_, _, e3, _), _), (_, (re1, _, _, _))) => e3 == re1
  }

  private def isHorizontalMatch(left: Edges, right: Edges) = (left, right) match {
    case (((_, e2, _, _), _), (_, (_, _, _, re4))) => e2 == re4
  }

  private val tileIdPattern = "Tile ([0-9]+):$".r
  private def parseInput(input: Seq[String]) = {
    val tileSize = input.tail.head.length
    input
      .grouped(tileSize + 2)
      .map { case tileIdPattern(tileId) :: tileData =>
        val grid = tileData.take(tileSize)
        (tileId.toInt, (grid, extractEdges(grid)))
      }
      .toMap
  }

  private def extractEdges(grid: Seq[String]) = {
    val (e1, e2, e3, e4) = (grid.head, grid.map(_.last).mkString, grid.last.reverse, grid.map(_.head).mkString.reverse)
    val (re1, re2, re3, re4) = (e1.reverse, e2.reverse, e3.reverse, e4.reverse)
    ((bitSet(e1), bitSet(e2), bitSet(e3), bitSet(e4)), (bitSet(re1), bitSet(re2), bitSet(re3), bitSet(re4)))
  }

  private def bitSet(s: String) =
    s.indices.foldLeft(0) { (bitset, i) =>
      if (s(i) == '#') bitset | (1 << i)
      else bitset
    }

  private def parseMonster(input: Seq[String]) =
    input.zipWithIndex.flatMap { case (s, x) =>
      s.zipWithIndex.collect { case (char, y) if char == '#' => (x, y) }
    }
}
