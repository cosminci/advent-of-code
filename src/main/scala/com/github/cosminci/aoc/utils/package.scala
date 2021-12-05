package com.github.cosminci.aoc

import scala.io.Source
import scala.util.Using

package object utils {
  def loadInputAsInts(path: String): Seq[Int] =
    loadInputAsStrings(path).map(_.toInt)

  def loadInputAsStrings(path: String): Seq[String] =
    Using.resource(Source.fromResource(path))(_.getLines().toSeq)
}
