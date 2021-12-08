package com.github.cosminci.aoc._2021

import com.github.cosminci.aoc.utils

object Day8 {
  def main(args: Array[String]): Unit = {
    val input = utils
      .loadInputAsStrings("2021/day8.txt")
      .map { line =>
        val Array(patterns, outputs) = line.split(" \\| ")
        (patterns.split(' ').toSeq, outputs.split(' ').toSeq)
      }

    println(s"Part I: ${countUniqueCombinationDigits(input)}")
    println(s"Part II: ${sumOutputValues(input)}")
  }

  def countUniqueCombinationDigits(input: Seq[(Seq[String], Seq[String])]): Int =
    input.flatMap(_._2).count(out => Set(2, 3, 4, 7).contains(out.length))

  def sumOutputValues(input: Seq[(Seq[String], Seq[String])]): Int = {
    val segmentsToDigits =
      Seq("abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg")
        .zip((0 to 9))
        .toMap

    def solve(combinations: Seq[String], outputs: Seq[String]): Int = {
      val Some((segmentA, segmentsCF, segmentsACF, segmentsBCDF)) = for {
        segmentsCF   <- combinations.find(_.length == 2).map(_.toSet) // One
        segmentsACF  <- combinations.find(_.length == 3).map(_.toSet) // Seven
        segmentsBCDF <- combinations.find(_.length == 4).map(_.toSet) // Four
      } yield (segmentsACF.diff(segmentsCF).head, segmentsCF, segmentsACF, segmentsBCDF)

      val segments   = ('a' to 'g').toSet
      val unknown    = combinations.filterNot(c => Set(2, 3, 4, 7).contains(c.length))
      val segmentE   = segments.filter(s => unknown.flatten.count(_ == s) == 3).head
      val segmentsAG = segments.filter(s => unknown.flatten.count(_ == s) == 6)
      val segmentG   = (segmentsAG - segmentA).head
      val segmentsBC = segments.filter(s => unknown.flatten.count(_ == s) == 4)
      val segmentC   = segmentsACF.intersect(segmentsBC).head
      val segmentB   = (segmentsBC - segmentC).head
      val segmentF   = (segmentsCF - segmentC).head
      val segmentsBD = segmentsBCDF.diff(segmentsACF)
      val segmentD   = (segmentsBD - segmentB).head

      val rewiring =
        Seq(segmentA, segmentB, segmentC, segmentD, segmentE, segmentF, segmentG)
          .zip('a' to 'g')
          .toMap

      outputs
        .map(out => segmentsToDigits(out.map(rewiring).sorted))
        .foldLeft(0)((v, digit) => v * 10 + digit)
    }

    input.map { case (combinations, output) => solve(combinations, output) }.sum
  }
}
