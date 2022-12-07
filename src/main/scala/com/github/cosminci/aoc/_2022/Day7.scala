package com.github.cosminci.aoc._2022

import com.github.cosminci.aoc.utils

import scala.util.chaining._

object Day7 {

  def main(args: Array[String]): Unit = {
    val input = utils.loadInputAsStrings("2022/day7.txt")
    val sizes = buildPathSizes(input)

    println(s"Part 1: ${sumDirectorySizesUnder(sizes, limit = 100_000)}")
    println(s"Part 2: ${smallestDirectoryToDelete(sizes, total = 70_000_000, minFree = 30_000_000)}")
  }

  def sumDirectorySizesUnder(sizes: Map[Seq[String], Int], limit: Int): Int =
    sizes.values.filter(_ <= limit).sum

  def smallestDirectoryToDelete(sizes: Map[Seq[String], Int], total: Int, minFree: Int): Int = {
    val used      = sizes(Seq("/"))
    val free      = total - used
    val minToFree = minFree - free
    sizes.values.filter(_ >= minToFree).min
  }

  private def buildPathSizes(input: Seq[String]): Map[Seq[String], Int] = {
    val initialPath  = Seq.empty[String]
    val initialSizes = Map.empty[Seq[String], Int].withDefaultValue(0)
    input
      .foldLeft(initialPath, initialSizes) { case ((path, sizes), line) => processLine(path, sizes, line) }
      .pipe { case (_, sizes) => sizes }
  }

  private def processLine(path: Seq[String], sizes: Map[Seq[String], Int], line: String) =
    line match {
      case s"dir $_"      => (path, sizes)
      case "$ ls"         => (path, sizes)
      case "$ cd .."      => (path.dropRight(1), sizes)
      case s"$$ cd $name" => (path.appended(name), sizes)
      case s"$size $_"    => (path, addSizeRecursively(path, sizes, size.toInt))
    }

  private def addSizeRecursively(path: Seq[String], sizes: Map[Seq[String], Int], toAdd: Int): Map[Seq[String], Int] =
    Iterator
      .iterate((path, sizes)) { case (path, sizes) => (path.dropRight(1), sizes.updated(path, sizes(path) + toAdd)) }
      .dropWhile { case (path, _) => path.nonEmpty }
      .next()
      .pipe { case (_, sizes) => sizes }

}
