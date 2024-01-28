package com.github.cosminci.aoc._2017

import com.github.cosminci.aoc.utils

import scala.collection.mutable

object Day21 {

  def main(args: Array[String]): Unit = {
    val rules = parseInput(utils.loadInputAsStrings("2017/day21.txt"))

    println(s"Part 1: ${pixelsOnAfterIters(rules, iters = 5)}")
    println(s"Part 2: ${pixelsOnAfterIters(rules, iters = 18)}")
  }

  private def pixelsOnAfterIters(rules: Map[Seq[String], Seq[String]], iters: Int) =
    Iterator
      .iterate(Seq(".#.", "..#", "###"))(grid => alignGrid(split(grid).map(transform(_, rules)).toSeq))
      .drop(iters).next()
      .map(_.count(_ == '#')).sum

  private val transformMem = mutable.Map.empty[Seq[String], Seq[String]]
  private def transform(pattern: Seq[String], rules: Map[Seq[String], Seq[String]]) =
    transformMem.getOrElseUpdate(
      pattern,
      rotationsAndFlips(pattern).collectFirst { case p if rules.contains(p) => rules(p) }.get
    )

  private def rotationsAndFlips(pattern: Seq[String]) = {
    val rotations = (1 to 3).scanLeft(pattern)((p, _) => rotateCW(p))
    val flips     = rotations.map(_.map(_.reverse.mkString))
    rotations ++ flips
  }

  private def rotateCW(pattern: Seq[String]) =
    pattern.transpose.map(_.reverse.mkString)

  private def split(pattern: Seq[String]) = {
    val size = if (pattern.length % 2 == 0) 2 else 3
    for {
      rows <- pattern.grouped(size)
      cols <- rows.transpose.grouped(size)
    } yield cols.map(_.mkString)
  }

  private def alignGrid(tiles: Seq[Seq[String]]): Seq[String] = {
    val size = math.sqrt(tiles.size).toInt
    tiles.grouped(size).flatMap(_.transpose.map(_.mkString)).toSeq
  }

  private def parseInput(input: Seq[String]) =
    input.map { case s"$from => $to" =>
      (from.split("/").map(_.mkString).toSeq, to.split("/").map(_.mkString).toSeq)
    }.toMap

}
