package com.github.cosminci.aoc._2025

import com.github.cosminci.aoc.utils.loadInputAsStrings

import scala.util.chaining.scalaUtilChainingOps

object Day6 {

  def main(args: Array[String]): Unit = {
    val input = loadInputAsStrings("2025/day6.txt")

    println(s"Part 1: ${equationResultSum(parseEquations1(input))}")
    println(s"Part 2: ${equationResultSum(parseEquations2(input.transpose.map(_.mkString)))}")
  }

  def parseEquations1(lines: Seq[String]) = {
    val operands  = lines.dropRight(1).map(_.split(' ').map(_.trim).filterNot(_.isEmpty).map(_.toLong))
    val operators = lines.last.split(' ').filterNot(_.isEmpty).map(_.trim.head)
    operands.transpose.zip(operators)
  }

  def parseEquations2(lines: Seq[String]) = {
    @annotation.tailrec
    def dfs(equations: Seq[(Seq[Long], Char)], i: Int): Seq[(Seq[Long], Char)] =
      if (i >= lines.length) equations
      else {
        val j        = lines.indexWhere(_.trim.isEmpty, from = i).pipe(i => if (i != -1) i else lines.length)
        val group    = lines.slice(i, j)
        val equation = (group.map(_.dropRight(1).trim.toLong), group.head.last)
        dfs(equations :+ equation, j + 1)
      }

    dfs(equations = Seq.empty, i = 0)
  }

  private def equationResultSum(equations: Seq[(Seq[Long], Char)]): Long =
    equations.map {
      case (operands, '+') => operands.sum
      case (operands, '*') => operands.product
    }.sum

}
