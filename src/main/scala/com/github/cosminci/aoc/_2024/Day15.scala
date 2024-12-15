package com.github.cosminci.aoc._2024

import com.github.cosminci.aoc.utils

import scala.util.chaining.scalaUtilChainingOps

object Day15 {

  def main(args: Array[String]): Unit = {
    val (robot, boxes, moves, grid) = parseInput(utils.loadInputAsStrings("2024/day15.txt"))

    println(s"Part 1: ${gpsCoordinatesSumSimple(robot, boxes, moves, grid)}")
    println(s"Part 2: ${gpsCoordinatesSumWidened(robot, boxes, moves, grid)}")
  }

  final case class Pos(r: Int, c: Int)
  final case class Boxes(idToPos: Map[Int, Seq[Pos]], posToId: Map[Pos, Int])

  def gpsCoordinatesSumSimple(robot: Pos, boxes: Map[Pos, Int], moves: Seq[(Int, Int)], grid: Seq[String]): Int = {
    val boxIdToPos = boxes.map { case (pos, id) => (id, Seq(pos)) }
    val boxPosToId = boxIdToPos.map { case (id, positions) => positions.head -> id }

    gpsCoordinatesSum(robot, Boxes(boxIdToPos, boxPosToId), moves, grid)
  }

  def gpsCoordinatesSumWidened(robot: Pos, boxes: Map[Pos, Int], moves: Seq[(Int, Int)], grid: Seq[String]): Int = {
    val widenedGrid = grid.map(row => row.zip(row).flatMap { case (ch1, ch2) => s"$ch1$ch2" }.mkString)
    val boxIdToPos = boxes.map { case (pos, id) => (id, Seq(pos.copy(c = pos.c * 2), pos.copy(c = pos.c * 2 + 1))) }
    val boxPosToId = boxIdToPos.flatMap { case (id, positions) => positions.map(_ -> id) }

    gpsCoordinatesSum(robot.copy(c = robot.c * 2), Boxes(boxIdToPos, boxPosToId), moves, widenedGrid)
  }

  private def gpsCoordinatesSum(robot: Pos, boxes: Boxes, moves: Seq[(Int, Int)], grid: Seq[String]) =
    moves.foldLeft(robot, boxes) { case ((robot, boxes), (dr, dc)) =>
      val next = Pos(robot.r + dr, robot.c + dc)
      if (grid(next.r)(next.c) == '#') (robot, boxes)
      else if (!boxes.posToId.contains(next)) (next, boxes)
      else {
        val boxesToMove = findBoxesToMove(boxes.posToId(next), boxes, grid, dr, dc)
        if (boxesToMove.isEmpty) (robot, boxes)
        else (next, moveBoxes(boxesToMove, boxes, dr, dc))
      }
    }.pipe { case (_, Boxes(idToPos, _)) => idToPos.values.map(p => p.head.r * 100 + p.head.c).sum }

  private def findBoxesToMove(boxId: Int, boxes: Boxes, grid: Seq[String], dr: Int, dc: Int) = {
    @annotation.tailrec
    def dfs(toVisit: Set[Int], visited: Set[Int]): Set[Int] = {
      val nextPositions = toVisit.flatMap(boxes.idToPos).map(p => Pos(p.r + dr, p.c + dc))
      if (nextPositions.exists(p => grid(p.r)(p.c) == '#')) Set.empty
      else {
        val nextBoxIds = nextPositions.flatMap(boxes.posToId.get).diff(toVisit)
        if (nextBoxIds.isEmpty) visited
        else dfs(nextBoxIds, visited ++ nextBoxIds)
      }
    }
    dfs(toVisit = Set(boxId), visited = Set(boxId))
  }

  private def moveBoxes(boxesToMove: Set[Int], boxes: Boxes, dr: Int, dc: Int) = {
    val movedIdToPos  = boxesToMove.map(id => id -> boxes.idToPos(id).map(pos => Pos(pos.r + dr, pos.c + dc)))
    val movedPosToIds = movedIdToPos.flatMap { case (id, pos) => pos.map(_ -> id) }
    val nextIdToPos   = boxes.idToPos -- boxesToMove ++ movedIdToPos
    val nextPosToId   = boxes.posToId -- boxesToMove.flatMap(boxes.idToPos) ++ movedPosToIds

    Boxes(nextIdToPos, nextPosToId)
  }

  private def parseInput(lines: Seq[String]) = {
    val directions    = Map('^' -> (-1, 0), 'v' -> (1, 0), '<' -> (0, -1), '>' -> (0, 1))
    val (grid, moves) = lines.splitAt(lines.indexWhere(_.isEmpty))
    val indices       = grid.indices.flatMap(r => grid(r).indices.map(c => (r, c)))
    val robot         = indices.collectFirst { case (r, c) if grid(r)(c) == '@' => Pos(r, c) }.get
    val boxes         = indices.collect { case (r, c) if grid(r)(c) == 'O' => Pos(r, c) }.toSet
    val cleanGrid     = grid.map(_.map(ch => if (ch == '@' || ch == 'O') '.' else ch))

    (robot, boxes.zipWithIndex.toMap, moves.tail.mkString.map(directions), cleanGrid)
  }

}
