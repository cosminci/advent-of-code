package com.github.cosminci.aoc._2024

import cats.syntax.all._
import com.github.cosminci.aoc.utils

object Day25 {

  def main(args: Array[String]): Unit = {
    val (locks, keys) = parseInput(utils.loadInputAsStrings("2024/day25.txt"))

    println(s"Part 1: ${uniqueNonOverlappingKeyLockPairs(locks, keys)}")
  }

  private val schematicHeight = 7

  def uniqueNonOverlappingKeyLockPairs(locks: Seq[Seq[Int]], keys: Seq[Seq[Int]]): Int =
    locks.map { lock =>
      keys.count { key =>
        lock.zip(key).forall { case (l, k) => l + k <= schematicHeight }
      }
    }.sum

  private def parseInput(lines: Seq[String]) =
    lines.grouped(schematicHeight + 1).map(_.dropRight(1)).toSeq.partitionEither { group =>
      if (group.head.contains("#")) parseSchematic(group).asLeft
      else parseSchematic(group).asRight
    }

  private def parseSchematic(lines: Seq[String]) =
    lines.transpose.map(_.reverse).map(_.count(_ == '#'))

}
