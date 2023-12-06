package com.github.cosminci.aoc

import cats.syntax.traverse._

import scala.io.Source
import scala.util.Using

package object utils {

  def loadInputAsInts(path: String): Seq[Int] =
    loadInputAsStrings(path).map(_.toInt)

  def loadInputAsLongs(path: String): Seq[Long] =
    loadInputAsStrings(path).map(_.toLong)

  def loadInputAsStrings(path: String): Seq[String] =
    Using.resource(Source.fromResource(path))(_.getLines().toSeq)

  def neighbours(m: Int, n: Int, r: Int, c: Int, includeDiagonals: Boolean = false): Seq[(Int, Int)] = {
    def collectWithinBounds(deltas: Seq[(Int, Int)]): Seq[(Int, Int)] =
      deltas.collect { case (dr, dc) if r + dr >= 0 && r + dr <= m && c + dc >= 0 && c + dc <= n => (r + dr, c + dc) }

    collectWithinBounds(Seq((-1, 0), (0, -1), (0, 1), (1, 0))) ++
      Option.when(includeDiagonals)(collectWithinBounds(Seq((-1, -1), (-1, 1), (1, -1), (1, 1)))).sequence.flatten
  }

  def counter[T](s: Iterable[T]): Map[T, Long] = s.groupMapReduce(identity)(_ => 1L)(_ + _)

}
