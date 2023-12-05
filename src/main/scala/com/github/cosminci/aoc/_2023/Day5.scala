package com.github.cosminci.aoc._2023

import com.github.cosminci.aoc.utils

import scala.util.chaining._

object Day5 {

  def main(args: Array[String]): Unit = {
    val input              = utils.loadInputAsStrings("2023/day5.txt")
    val (seeds, rangeMaps) = parseInput(input)

    println(s"Part 1: ${lowestLocationNumber(seeds, rangeMaps)}")
    println(s"Part 2: ${lowestLocationNumberRanges(seeds, rangeMaps)}")
  }

  final case class Range(start: Long, end: Long)

  def lowestLocationNumber(seeds: Seq[Long], categoryRangeMaps: Seq[Map[Range, Long]]): Long =
    seeds
      .map(seedStart => Range(seedStart, seedStart + 1))
      .pipe(lowestLocation(_, categoryRangeMaps))

  def lowestLocationNumberRanges(seeds: Seq[Long], categoryRangeMaps: Seq[Map[Range, Long]]): Long =
    seeds
      .grouped(2).map { case Seq(seedStart, seedLen) => Range(seedStart, seedStart + seedLen) }
      .pipe(ranges => lowestLocation(ranges.toSeq, categoryRangeMaps))

  private def lowestLocation(seedRanges: Seq[Range], categoryRangeMaps: Seq[Map[Range, Long]]) =
    seedRanges.flatMap(foldCategories(_, categoryRangeMaps)).map(_.start).min

  private def foldCategories(seedRange: Range, categoryRangeMaps: Seq[Map[Range, Long]]) =
    categoryRangeMaps.foldLeft(Seq(seedRange)) { (sourceRanges, rangeMaps) =>
      sourceRanges.flatMap(getOutputRanges(_, rangeMaps).toSeq)
    }

  private def getOutputRanges(input: Range, rangeMaps: Map[Range, Long]) = {
    val (mappedInputRanges, outputRanges) = rangeMaps.flatMap { case (src: Range, offset: Long) =>
      val start  = src.start.max(input.start)
      val end    = src.end.min(input.end)
      Option.when(start <= end)(Range(start, end), Range(start + offset, end + offset))
    }.unzip

    val cuts                = input.start +: mappedInputRanges.toSeq.flatMap(r => Seq(r.start, r.end)) :+ input.end
    val unmappedInputRanges = cuts.grouped(2).flatMap { case Seq(i, j) => Option.when(j > i)(Range(i, j)) }

    unmappedInputRanges ++ outputRanges
  }

  private def parseInput(input: Seq[String]) = {
    val (seedLines, mapLines) = input.splitAt(2)

    val seeds = seedLines.head.replace("seeds: ", "").split(' ').map(_.toLong).toSeq

    val rangeMaps = Seq("seed", "soil", "fertilizer", "water", "light", "temperature", "humidity").map {
      sourceCategory =>
        val start = mapLines.indexWhere(_.startsWith(sourceCategory))
        val end   = mapLines.indexWhere(_.isEmpty, start).pipe(i => if (i == -1) mapLines.length else i)
        mapLines
          .slice(start + 1, end)
          .map(_.split(' ').map(_.toLong))
          .map { case Array(dst, src, len) => Range(src, src + len) -> (dst - src) }
          .toMap
    }

    (seeds, rangeMaps)
  }

}
