package com.github.cosminci.aoc._2023

import com.github.cosminci.aoc.utils

object Day13 {

  def main(args: Array[String]): Unit = {
    val input    = utils.loadInputAsStrings("2023/day13.txt")
    val patterns = parsePatterns(input)

    println(s"Part 1: ${reflectionScore(patterns)}")
    println(s"Part 2: ${secondReflectionScore(patterns)}")
  }

  sealed trait Reflection
  case class Horizontal(row: Int)  extends Reflection
  case class Vertical(column: Int) extends Reflection

  def reflectionScore(patterns: Seq[Seq[Seq[Char]]]): Int =
    patterns.flatMap(findReflections(_).headOption).map(scoreReflection).sum

  def secondReflectionScore(patterns: Seq[Seq[Seq[Char]]]): Int =
    patterns.flatMap(findSecondReflection).map(scoreReflection).sum

  private def scoreReflection(r: Reflection) = r match {
    case Horizontal(row) => 100 * row
    case Vertical(col)   => col
  }

  private def findReflections(pattern: Seq[Seq[Char]]) = {
    val horizontal = findHorizontal(pattern).map(Horizontal)
    val vertical   = findHorizontal(pattern.transpose).map(Vertical)
    horizontal ++ vertical
  }

  private def findHorizontal(pattern: Seq[Seq[Char]]) =
    (1 until pattern.length).filter { splitIndex =>
      val (fh, sh) = pattern.splitAt(splitIndex)
      val size     = splitIndex.min(pattern.length - splitIndex)
      fh.takeRight(size) == sh.take(size).reverse
    }.toSet

  private def findSecondReflection(pattern: Seq[Seq[Char]]) = {
    val originalReflection = findReflections(pattern).head
    smudgeArrangements(pattern)
      .map(p => findReflections(p) - originalReflection)
      .collectFirst { case s if s.nonEmpty => s.head }
  }

  private def smudgeArrangements(pattern: Seq[Seq[Char]]) =
    pattern.indices.iterator.flatMap { r =>
      pattern(r).indices.iterator.map { c =>
        pattern.updated(r, pattern(r).updated(c, opposite(pattern(r)(c))))
      }
    }

  private def opposite(ch: Char): Char = ch match {
    case '#' => '.'
    case '.' => '#'
  }

  private def parsePatterns(input: Seq[String]) =
    input.foldLeft(Seq(Seq.empty[Seq[Char]])) { (acc, line) =>
      if (line.isEmpty) acc :+ Seq.empty[Seq[Char]]
      else acc.dropRight(1) :+ (acc.last :+ line)
    }

}
