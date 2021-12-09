package com.github.cosminci.aoc

import scala.io.Source
import scala.util.Using

package object utils {
  def loadInputAsInts(path: String): Seq[Int] =
    loadInputAsStrings(path).map(_.toInt)

  def loadInputAsStrings(path: String): Seq[String] =
    Using.resource(Source.fromResource(path))(_.getLines().toSeq)

  def neighbours(grid: Seq[Seq[Int]], r: Int, c: Int): Seq[(Int, Int)] = {
    val upper = Option.when(r > 0)((r - 1, c))
    val lower = Option.when(r < grid.length - 1)((r + 1, c))
    val left  = Option.when(c > 0)((r, c - 1))
    val right = Option.when(c < grid(r).length - 1)((r, c + 1))
    Seq.empty ++ upper ++ lower ++ left ++ right
  }
}
