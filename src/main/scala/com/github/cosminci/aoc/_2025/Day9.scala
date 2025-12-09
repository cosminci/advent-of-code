package com.github.cosminci.aoc._2025

import com.github.cosminci.aoc.utils.loadInputAsStrings

object Day9 {

  def main(args: Array[String]): Unit = {
    val tiles = parseInput(loadInputAsStrings("2025/day9.txt"))

    println(s"Part 1: ${largestRectangle(tiles)}")
    println(s"Part 2: ${largestRectangleWithinPolygon(tiles)}")
  }

  final case class Pos(r: Long, c: Long)

  def largestRectangle(tiles: Seq[Pos]): Long =
    tiles.combinations(2).map { case Seq(p1, p2) => ((p2.r - p1.r).abs + 1) * ((p2.c - p1.c).abs + 1) }.max

  def largestRectangleWithinPolygon(tiles: Seq[Pos]): Long = {
    val (rowMap, colMap)        = compressCoordinates(tiles)
    val (gridHeight, gridWidth) = (rowMap.size * 2 + 2, colMap.size * 2 + 2)
    val polygonBoundary         = buildBoundary(tiles, rowMap, colMap).toSet
    val outsideCells            = floodFillOutside(gridHeight, gridWidth, polygonBoundary)
    val prefixSum               = buildPrefixSum(gridHeight, gridWidth, outsideCells)

    def containsOutsideCells(p1: Pos, p2: Pos) = {
      val (minR, maxR) = (rowMap(p1.r.min(p2.r)), rowMap(p1.r.max(p2.r)))
      val (minC, maxC) = (colMap(p1.c.min(p2.c)), colMap(p1.c.max(p2.c)))
      prefixSum(maxR + 1)(maxC + 1) - prefixSum(minR)(maxC + 1) - prefixSum(maxR + 1)(minC) + prefixSum(minR)(minC) > 0
    }

    tiles
      .combinations(2)
      .filterNot { case Seq(p1, p2) => containsOutsideCells(p1, p2) }
      .map { case Seq(p1, p2) => ((p2.r - p1.r).abs + 1) * ((p2.c - p1.c).abs + 1) }
      .max
  }

  private def compressCoordinates(tiles: Seq[Pos]) = {
    val rowMap = tiles.map(_.r).distinct.sorted.zipWithIndex.map { case (r, i) => r -> (i * 2 + 1) }.toMap
    val colMap = tiles.map(_.c).distinct.sorted.zipWithIndex.map { case (c, i) => c -> (i * 2 + 1) }.toMap
    (rowMap, colMap)
  }

  private def buildBoundary(tiles: Seq[Pos], rowMap: Map[Long, Int], colMap: Map[Long, Int]) =
    (tiles :+ tiles.head)
      .sliding(2)
      .flatMap { case Seq(v1, v2) =>
        val (r1, c1, r2, c2) = (rowMap(v1.r), colMap(v1.c), rowMap(v2.r), colMap(v2.c))
        if (r1 == r2) (c1.min(c2) to c1.max(c2)).map(c => Pos(r1, c))
        else (r1.min(r2) to r1.max(r2)).map(r => Pos(r, c1))
      }

  private def floodFillOutside(gridHeight: Int, gridWidth: Int, boundary: Set[Pos]) = {
    def findNeighbors(pos: Pos, visited: Set[Pos]) =
      Seq((0, 1), (0, -1), (1, 0), (-1, 0))
        .map { case (dr, dc) => Pos(pos.r + dr, pos.c + dc) }
        .filter(neighbor => neighbor.r >= 0 && neighbor.r < gridHeight && neighbor.c >= 0 && neighbor.c < gridWidth)
        .filterNot(neighbor => boundary.contains(neighbor) || visited.contains(neighbor))

    @annotation.tailrec
    def dfs(toVisit: Seq[Pos], visited: Set[Pos]): Set[Pos] =
      if (toVisit.isEmpty) visited
      else {
        val neighbors = toVisit.flatMap(findNeighbors(_, visited)).distinct
        dfs(neighbors, visited ++ neighbors)
      }

    dfs(toVisit = Seq(Pos(0, 0)), visited = Set(Pos(0, 0)))
  }

  private def buildPrefixSum(gridHeight: Int, gridWidth: Int, outsideCells: Set[Pos]) =
    (0 until gridHeight).scanLeft(Seq.fill(gridWidth + 1)(0)) { (prevRow, r) =>
      (0 until gridWidth).scanLeft(0) { (left, c) =>
        (if (outsideCells.contains(Pos(r, c))) 1 else 0) + left + prevRow(c + 1) - prevRow(c)
      }
    }

  private def parseInput(lines: Seq[String]) =
    lines.map(_.split(',').map(_.toLong)).map { case Array(r, c) => Pos(r, c) }

}
