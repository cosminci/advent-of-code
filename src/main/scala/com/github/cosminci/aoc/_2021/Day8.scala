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

  def sumOutputValues(input: Seq[(Seq[String], Seq[String])]): Int =
    input.flatMap { case (combinations, outputs) =>
      deduceMappings(combinations).map { mappings =>
        outputs
          .map(o => mappings(o.toSet))
          .foldLeft(0)((v, digit) => v * 10 + digit)
      }
    }.sum

  private def deduceMappings(combinations: Seq[String]) =
    for {
      one   <- combinations.find(_.length == 2)
      seven <- combinations.find(_.length == 3)
      four  <- combinations.find(_.length == 4)
      eight <- combinations.find(_.length == 7)
      six   <- combinations.find(c => c.length == 6 && c.diff(one).length == 5)
      nine  <- combinations.find(c => c.length == 6 && c.diff(four).length == 2)
      zero  <- combinations.find(c => c.length == 6 && c != six && c != nine)
      three <- combinations.find(c => c.length == 5 && c.diff(one).length == 3)
      two   <- combinations.find(c => c.length == 5 && c.diff(nine).length == 1)
      five  <- combinations.find(c => c.length == 5 && c != two && c != three)
    } yield Seq(zero, one, two, three, four, five, six, seven, eight, nine)
      .map(_.toSet)
      .zip(0 to 9)
      .toMap
}
