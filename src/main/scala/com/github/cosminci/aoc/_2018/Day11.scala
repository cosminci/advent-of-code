package com.github.cosminci.aoc._2018

import scala.util.chaining._

object Day11 {

  def main(args: Array[String]): Unit = {
    val serialNumber = 9995

    println(s"Part 1: ${largestPower3x3Square(serialNumber)}")
    println(s"Part 2: ${largestPowerAnySizeSquare(serialNumber)}")
  }

  private val gridSize = 300

  def largestPower3x3Square(serialNumber: Int): String = {
    val powerGrid  = buildPowerGrid(serialNumber)
    val prefixSums = buildPrefixSums(powerGrid)

    powerLevelsForSize(size = 3, prefixSums)
      .maxBy { case (_, _, power) => power }
      .pipe { case (x, y, _) => s"${x + 1},${y + 1}" }
  }

  def largestPowerAnySizeSquare(serialNumber: Int): String = {
    val powerGrid  = buildPowerGrid(serialNumber)
    val prefixSums = buildPrefixSums(powerGrid)

    (1 to 300)
      .flatMap(size => powerLevelsForSize(size, prefixSums).map(size -> _))
      .maxBy { case (_, (_, _, power)) => power }
      .pipe { case (size, (x, y, _)) => s"${x + 1},${y + 1},$size" }
  }

  private def powerLevelsForSize(size: Int, prefixSums: Seq[Seq[Int]]) =
    for {
      x <- 1 to gridSize - size
      y <- 1 to gridSize - size
    } yield (x, y, powerLevel(x, y, size, prefixSums))

  private def buildPrefixSums(grid: Seq[Seq[Int]]) =
    Array.ofDim[Int](gridSize + 1, gridSize + 1)
      .tap { acc =>
        for {
          x <- 1 to gridSize
          y <- 1 to gridSize
        } acc(x)(y) = grid(x - 1)(y - 1) + acc(x - 1)(y) + acc(x)(y - 1) - acc(x - 1)(y - 1)
      }.map(_.toSeq).toSeq

  private def powerLevel(x: Int, y: Int, size: Int, prefixSums: Seq[Seq[Int]]) =
    prefixSums(x + size)(y + size) - prefixSums(x)(y + size) - prefixSums(x + size)(y) + prefixSums(x)(y)

  private def buildPowerGrid(serialNumber: Int) =
    Seq.tabulate(gridSize, gridSize) { case (x, y) =>
      val rackId = x + 11
      val power  = (rackId * (y + 1) + serialNumber) * rackId
      ((power / 100) % 10) - 5
    }

}
