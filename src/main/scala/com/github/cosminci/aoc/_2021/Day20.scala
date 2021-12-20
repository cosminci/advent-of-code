package com.github.cosminci.aoc._2021

import com.github.cosminci.aoc.utils

object Day20 {
  def main(args: Array[String]): Unit = {
    val input         = utils.loadInputAsStrings("2021/day20.txt")
    val (algo, image) = (input.head, input.drop(2))

    println(s"Part I: ${countLitPixels(image, algo, enhancementCount = 2)}")
    println(s"Part II: ${countLitPixels(image, algo, enhancementCount = 50)}")
  }

  def countLitPixels(image: Seq[String], algo: String, enhancementCount: Int): Int =
    (1 to enhancementCount)
      .foldLeft((image, '.')) { case ((image, outer), _) => enhance(image, outer, algo) }._1
      .foldLeft(0)((count, line) => count + line.count(_ == '#'))

  private def enhance(image: Seq[String], outer: Char, algo: String): (Seq[String], Char) = {
    val paddedImage = padImage(image, outer)

    def applyFilter(i: Int, j: Int): Char = {
      val chars = for {
        i <- i - 1 to i + 1
        j <- j - 1 to j + 1
      } yield Option.when(paddedImage(i)(j) == '#')('1').getOrElse('0')

      algo(Integer.parseInt(chars.mkString, 2))
    }

    val (m, n) = (paddedImage.length, paddedImage.head.length)
    val pixels = for {
      i <- 1 until m - 1
      j <- 1 until n - 1
    } yield applyFilter(i, j)

    val newOuter = Option.when(algo.head == '.')('.').getOrElse(Option.when(outer == '.')('#').getOrElse('.'))

    (pixels.grouped(n - 2).map(_.mkString).toSeq, newOuter)
  }

  private def padImage(image: Seq[String], outer: Char): Seq[String] = {
    val (m, n) = (image.length, image.head.length)

    val leftPad   = (2 - image.map(_.indexWhere(_ != '#')).min).max(0)
    val rightPad  = (n + 1 - image.map(_.lastIndexWhere(_ == '#')).max).max(0)
    val topPad    = (2 - image.indexWhere(_.contains('#'))).max(0)
    val bottomPad = (m + 1 - image.lastIndexWhere(_.contains('#'))).max(0)

    val width = n + leftPad + rightPad
    Seq.fill(topPad)(outer.toString * width) ++
      image.map(line => s"${outer.toString * leftPad}$line${outer.toString * rightPad}") ++
      Seq.fill(bottomPad)(outer.toString * width)
  }
}
