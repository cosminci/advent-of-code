package com.github.cosminci.aoc._2024

import com.github.cosminci.aoc.utils

import scala.collection.mutable

object Day19 {

  def main(args: Array[String]): Unit = {
    val (towels, designs) = parseInput(utils.loadInputAsStrings("2024/day19.txt"))

    println(s"Part 1: ${possibleDesignCount(designs, towels)}")
    println(s"Part 2: ${totalWaysToCreateDesigns(designs, towels)}")
  }

  def possibleDesignCount(designs: Seq[String], towels: Seq[String]): Int =
    designs.count(d => waysToCreate(d, towels) > 0)

  def totalWaysToCreateDesigns(designs: Seq[String], towels: Seq[String]): Long =
    designs.map(d => waysToCreate(d, towels)).sum

  private def waysToCreate(design: String, towels: Seq[String]) = {
    val mem = mutable.Map.empty[Int, Long]
    def dfs(i: Int): Long = mem.getOrElseUpdate(i,
      if (i == design.length) 1
      else towels.collect { case t if design.startsWith(t, i) => dfs(i + t.length) }.sum
    )
    dfs(i = 0)
  }

  private def parseInput(lines: Seq[String]) = {
    val towels   = lines.head.split(", ").toSeq
    val patterns = lines.drop(2)
    (towels, patterns)
  }

}
