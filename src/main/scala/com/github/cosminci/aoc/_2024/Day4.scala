package com.github.cosminci.aoc._2024

import com.github.cosminci.aoc.utils

import scala.util.chaining.scalaUtilChainingOps

object Day4 {

  def main(args: Array[String]): Unit = {
    val wordSearch = utils.loadInputAsStrings("2024/day4.txt")

    println(s"Part 1: ${xmasCount(wordSearch)}")
    println(s"Part 2: ${doubleMasCount(wordSearch)}")
  }

  def xmasCount(wordSearch: Seq[String]): Int = {
    val startSeeds = for {
      (r, c)   <- indices(wordSearch)
      (dr, dc) <- directions
    } yield (r, c, dr, dc)

    startSeeds.count { case (r, c, dr, dc) => matchesXmas(r, c, dr, dc, wordSearch) }
  }

  def doubleMasCount(wordSearch: Seq[String]): Int = {
    val startSeeds = for {
      (r, c) <- indices(wordSearch)
      if r.min(c) > 0 && r.max(c) < wordSearch.length - 1
    } yield (r, c)

    startSeeds.count { case (r, c) => wordSearch(r)(c) == 'A' && matchesDoubleMas(r, c, wordSearch) }
  }

  private val xmasTarget = "XMAS"
  private def matchesXmas(r: Int, c: Int, dr: Int, dc: Int, wordSearch: Seq[String]) = {
    @annotation.tailrec
    def dfs(r: Int, c: Int, i: Int): Boolean = {
      if (i == xmasTarget.length) true
      else if (r.min(c) < 0 || r.max(c) == wordSearch.length) false
      else if (wordSearch(r)(c) != xmasTarget(i)) false
      else dfs(r + dr, c + dc, i + 1)
    }
    dfs(r, c, i = 0)
  }

  private val doubleMasTargets = Iterator.iterate(Seq("MS", "MS"))(rotateCW).map(_.flatten).take(4).toSeq
  private val doubleMasCoords  = Seq((-1, -1), (-1, 1), (1, -1), (1, 1))
  private def matchesDoubleMas(r: Int, c: Int, wordSearch: Seq[String]) =
    doubleMasTargets.contains(doubleMasCoords.flatMap { case (dr, dc) => Seq(wordSearch(r + dr)(c + dc)) })

  private def indices(grid: Seq[String]) = for {
    r <- grid.indices
    c <- grid(r).indices
  } yield (r, c)

  private val directions = for {
    dr <- -1 to 1
    dc <- -1 to 1
    if dr != 0 || dc != 0
  } yield (dr, dc)

  private def rotateCW(pattern: Seq[String]) =
    pattern.transpose.map(_.reverse.mkString)

}
