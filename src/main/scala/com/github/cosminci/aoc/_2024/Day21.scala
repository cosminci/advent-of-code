package com.github.cosminci.aoc._2024

import com.github.cosminci.aoc.utils

import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps

object Day21 {

  def main(args: Array[String]): Unit = {
    val codes = utils.loadInputAsStrings("2024/day21.txt")

    println(s"Part 1: ${codes.map(codeComplexity(_, totalDirectionalKeypads = 2)).sum}")
    println(s"Part 2: ${codes.map(codeComplexity(_, totalDirectionalKeypads = 25)).sum}")
  }

  final case class Pos(r: Int, c: Int)

  private val numericKeyCoords = Map(
    '7' -> Pos(0, 0), '8' -> Pos(0, 1), '9' -> Pos(0, 2),
    '4' -> Pos(1, 0), '5' -> Pos(1, 1), '6' -> Pos(1, 2),
    '1' -> Pos(2, 0), '2' -> Pos(2, 1), '3' -> Pos(2, 2),
    '0' -> Pos(3, 1), 'A' -> Pos(3, 2)
  )
  private val directionalKeyCoords = Map(
    '^' -> Pos(0, 1), 'A' -> Pos(0, 2),
    '<' -> Pos(1, 0), 'v' -> Pos(1, 1), '>' -> Pos(1, 2),
  )
  private val deltas = Map(
    '^' -> (-1, 0), 'v' -> (1, 0), '<' -> (0, -1), '>' -> (0, 1)
  )

  def codeComplexity(code: String, totalDirectionalKeypads: Int): Long =
    code.take(3).toInt * shortestPathToType(code, keypadIdx = 0, totalDirectionalKeypads)

  private val mem = mutable.Map.empty[(String, Int, Int), Long]
  private def shortestPathToType(code: String, keypadIdx: Int, totalDirectionalKeypads: Int): Long =
    mem.getOrElseUpdate((code, keypadIdx, totalDirectionalKeypads), {
      val keyCoords  = if (keypadIdx == 0) numericKeyCoords else directionalKeyCoords
      val invalidPos = if (keypadIdx == 0) Pos(3, 0) else Pos(0, 0)
      code.map(keyCoords).foldLeft(0L, keyCoords('A')) { case ((steps, curr), next) =>
        val paths = shortestPathsBetween(curr, next, invalidPos)
        if (keypadIdx == totalDirectionalKeypads) (steps + paths.head.length, next)
        else (steps + paths.map(shortestPathToType(_, keypadIdx + 1, totalDirectionalKeypads)).min, next)
      }.pipe { case (totalSteps, _) => totalSteps }
    })

  private def shortestPathsBetween(start: Pos, end: Pos, invalidPos: Pos) = {
    val (dr, dc)        = (end.r - start.r, end.c - start.c)
    val horizontalMoves = if (dr < 0) "^" * dr.abs else "v" * dr
    val verticalMoves   = if (dc < 0) "<" * dc.abs else ">" * dc

    (horizontalMoves ++ verticalMoves).permutations.filter(isValid(_, start, invalidPos)).toSeq.map(_ :+ 'A')
  }

  private def isValid(moves: String, start: Pos, invalidPos: Pos) =
    !moves.map(deltas).scanLeft(start) { case (p, (dr, dc)) => Pos(p.r + dr, p.c + dc) }.contains(invalidPos)

}
