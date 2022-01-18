package com.github.cosminci.aoc._2020

import com.github.cosminci.aoc.utils

object Day14 {

  def main(args: Array[String]): Unit = {
    val inputData         = utils.loadInputAsStrings("2020/day14.txt")
    val instructionGroups = parseInput(inputData)

    println(s"Part 1: ${applyInstructions(instructionGroups).values.sum}")
    println(s"Part 2: ${applyInstructionsFloating(instructionGroups).values.sum}")
  }

  type InstructionGroup = (Map[Int, Char], Seq[(Long, Long)])

  def applyInstructions(instructionGroups: Seq[InstructionGroup]): Map[Long, Long] =
    instructionGroups
      .foldLeft(Map.empty[Long, Long]) { case (memory, (mask, instructions)) =>
        val valueMask = mask.collect { case (idx, bit) if bit != 'X' => (idx, bit - '0') }
        instructions.foldLeft(memory) { case (memory, (location, value)) =>
          memory.updated(location, applyMaskToValue(valueMask, value))
        }
      }

  private def applyMaskToValue(mask: Map[Int, Int], n: Long): Long =
    mask.foldLeft(n) { case (n, (idx, bit)) =>
      if (bit == 0) n & ~(1L << idx)
      else n | (1L << idx)
    }

  def applyInstructionsFloating(instructionGroups: Seq[InstructionGroup]): Map[Long, Long] =
    instructionGroups
      .foldLeft(Map.empty[Long, Long]) { case (memory, (mask, instructions)) =>
        instructions.foldLeft(memory) { case (memory, (location, value)) =>
          expandLocationMask(applyMaskToLocation(mask, location)).foldLeft(memory) { case (memory, location) =>
            memory.updated(location, value)
          }
        }
      }

  private def expandLocationMask(s: String): Seq[Long] =
    s.indices
      .filter(i => s(i) == 'X')
      .foldLeft(Seq(s))((locs, i) => locs.flatMap(l => Seq(l.updated(i, '0'), l.updated(i, '1'))))
      .map(v => java.lang.Long.parseLong(v, 2))

  private def applyMaskToLocation(mask: Map[Int, Char], n: Long): String =
    mask
      .filter { case (_, bit) => bit != '0' }
      .foldLeft(n.toBinaryString.reverse.padTo(36, '0')) { case (n, (idx, bit)) =>
        n.updated(idx, bit)
      }
      .reverse

  private val maskPattern   = "mask = ([01X]+)?".r
  private val memoryPattern = "mem\\[([0-9]+)] = ([0-9]+)?".r

  private def parseInput(input: Seq[String]) = {
    val maskIndices = input.indices.filter(i => maskPattern.matches(input(i)))

    (maskIndices.sliding(2).toSeq :+ Seq(maskIndices.last, input.length)).map { case Seq(maskIdx, groupEndIdx) =>
      val mask = input(maskIdx) match {
        case maskPattern(s) =>
          s.reverse.zipWithIndex.map { case (bit, idx) => (idx, bit) }.toMap
      }
      val instructions = input.slice(maskIdx + 1, groupEndIdx).map { case memoryPattern(location, value) =>
        (location.toLong, value.toLong)
      }
      (mask, instructions)
    }
  }
}
