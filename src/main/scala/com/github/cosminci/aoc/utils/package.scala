package com.github.cosminci.aoc

import scala.io.Source
import scala.util.Using

package object utils {
  def loadInputAsInts(path: String): Seq[Int] =
    loadInputAsStrings(path).map(_.toInt)

  def loadInputAsStrings(path: String): Seq[String] =
    Using.resource(Source.fromResource(path))(_.getLines().toSeq)

  def neighbours(n: Int, m: Int, r: Int, c: Int, includeDiagonals: Boolean = false): Seq[(Int, Int)] = {
    val upper = Option.when(r > 0)((r - 1, c))
    val lower = Option.when(r < n)((r + 1, c))
    val left  = Option.when(c > 0)((r, c - 1))
    val right = Option.when(c < m)((r, c + 1))

    val upperLeft  = Option.when(includeDiagonals && r > 0 && c > 0)((r - 1, c - 1))
    val upperRight = Option.when(includeDiagonals && r > 0 && c < m)((r - 1, c + 1))
    val lowerLeft  = Option.when(includeDiagonals && r < n && c > 0)((r + 1, c - 1))
    val lowerRight = Option.when(includeDiagonals && r < n && c < m)((r + 1, c + 1))

    Seq.empty ++ upper ++ lower ++ left ++ right ++ upperLeft ++ upperRight ++ lowerLeft ++ lowerRight
  }
}
