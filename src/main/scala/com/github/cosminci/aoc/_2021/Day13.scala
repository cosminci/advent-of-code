package com.github.cosminci.aoc._2021

import com.github.cosminci.aoc.utils

object Day13 {
  def main(args: Array[String]): Unit = {
    val input = utils.loadInputAsStrings("2021/day13.txt")

    val dots = input
      .takeWhile(_.nonEmpty)
      .map(_.split(','))
      .map { case Array(x, y) => (x.toInt, y.toInt) }
      .toSet

    val folds = input
      .slice(dots.size + 1, input.length)
      .map(_.split(' ').last.split('='))
      .map { case Array(axis, line) => (axis, line.toInt) }

    println(s"Part I: ${fold(dots, folds.head).size}")
    println(s"Part II: \n${draw(folds.foldLeft(dots)(fold))}")
  }

  private def fold(dots: Set[(Int, Int)], fold: (String, Int)): Set[(Int, Int)] =
    fold match {
      case (foldAxis, foldLine) =>
        val foldCoordinate = (c: Int) => Option.when(c > foldLine)(2 * foldLine - c).getOrElse(c)
        Option
          .when(foldAxis == "x")(dots.map { case (x, y) => (foldCoordinate(x), y) })
          .getOrElse(dots.map { case (x, y) => (x, foldCoordinate(y)) })
    }

  private def draw(dots: Set[(Int, Int)]): String = {
    val (xSize, ySize) = dots.foldLeft(0, 0) { case ((xSize, ySize), (x, y)) => (xSize.max(x + 1), ySize.max(y + 1)) }
    Seq
      .tabulate(xSize, ySize) { case (x, y) => Option.when(dots.contains(x, y))("O").getOrElse(" ") * 3 }
      .transpose
      .map(_.mkString)
      .mkString("\n")
  }
}
