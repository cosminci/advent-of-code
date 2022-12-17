package com.github.cosminci.aoc._2022

import com.github.cosminci.aoc.utils

import scala.annotation.tailrec
import scala.math.Integral.Implicits.infixIntegralOps
import scala.util.chaining._

object Day17 {

  def main(args: Array[String]): Unit = {
    val input       = utils.loadInputAsStrings("2022/day17.txt").head
    val windPattern = input.map(d => if (d == '>') 1 else -1)

    println(s"Part 1: ${heightAfterRockfall(windPattern, rockTarget = 2022)}")
    println(s"Part 2: ${heightAfterRockfall(windPattern, rockTarget = 1_000_000_000_000L)}")
  }

  type Pos   = (Int, Long)
  type Shape = Seq[Pos]
  type Cache = Map[Set[Pos], (Long, Long)]

  private val shapes: Seq[Shape] = Seq(
    Seq((2, 4), (3, 4), (4, 4), (5, 4)),
    Seq((3, 6), (2, 5), (3, 5), (4, 5), (3, 4)),
    Seq((4, 6), (4, 5), (2, 4), (3, 4), (4, 4)),
    Seq((2, 7), (2, 6), (2, 5), (2, 4)),
    Seq((2, 5), (3, 5), (2, 4), (3, 4))
  )

  private val ChamberWidth    = 7
  private val CacheHeightSize = 50

  case class State(shape: Shape, shapeIdx: Int, windIdx: Int, height: Long, rockCount: Long, placed: Set[Pos])

  def heightAfterRockfall(windPattern: Seq[Int], rockTarget: Long): Long = {
    @tailrec
    def dfs(state: State, cache: Cache): Long =
      move(state.shape, windPattern(state.windIdx), state.placed) match {
        case Right(fallingShape) =>
          dfs(state.copy(shape = fallingShape, windIdx = (state.windIdx + 1) % windPattern.length), cache)
        case Left(stoppedShape) =>
          val newState = nextState(state, stoppedShape, windPattern)
          tryToFindCycle(rockTarget, newState, cache) match {
            case Some(finalHeight) => finalHeight
            case None => dfs(newState, cache + (normalize(state.placed) -> (state.rockCount, state.height)))
          }
      }
    val initialPlaced = (0 until ChamberWidth).map(x => (x, 0L)).toSet
    val initialState  = State(shapes.head, shapeIdx = 0, windIdx = 0, height = 0, rockCount = 0, initialPlaced)
    dfs(initialState, cache = Map.empty)
  }

  private def nextState(state: State, stoppedShape: Shape, windPattern: Seq[Int]) = {
    val newHeight   = state.height.max(stoppedShape.map { case (_, y) => y }.max)
    val newShapeIdx = (state.shapeIdx + 1) % shapes.length
    val newWindIdx  = (state.windIdx + 1)  % windPattern.length
    val newShape    = shapes(newShapeIdx).map { case (x, y) => (x, newHeight + y) }
    val newPlaced   = (state.placed ++ stoppedShape).filter { case (_, y) => newHeight - y <= CacheHeightSize }
    State(newShape, newShapeIdx, newWindIdx, newHeight, state.rockCount + 1, newPlaced)
  }

  private def tryToFindCycle(rockTarget: Long, state: State, cache: Cache) =
    cache.get(normalize(state.placed)).flatMap { case (prevRockCount, prevHeight) =>
      val rocksPerCycle                    = state.rockCount - prevRockCount
      val (cyclesNeeded, extraRocksNeeded) = (rockTarget - state.rockCount) /% rocksPerCycle
      cache.values
        .collectFirst { case (rocks, height) if rocks == prevRockCount + extraRocksNeeded => height }
        .map(_ + (state.height - prevHeight) * (cyclesNeeded + 1))
    }

  private def normalize(placed: Set[Pos]) =
    placed.map { case (_, y) => y }.min.pipe(minY => placed.map { case (x, y) => (x, y - minY) })

  private def move(shape: Shape, dx: Int, placed: Set[Pos]) = {
    val movedLateral = shape.map { case (x, y) => (x + dx, y) }
    val afterLateral = if (movedLateral.forall(isValidPos(_, placed))) movedLateral else shape
    val movedDown    = afterLateral.map { case (x, y) => (x, y - 1) }
    Either.cond(movedDown.forall(isValidPos(_, placed)), movedDown, afterLateral)
  }

  private def isValidPos(pos: Pos, placed: Set[Pos]) =
    pos.pipe { case (x, _) => x >= 0 && x < ChamberWidth && !placed.contains(pos) }

}
