package com.github.cosminci.aoc._2024

import com.github.cosminci.aoc.utils

object Day7 {

  def main(args: Array[String]): Unit = {
    val equations = parseInput(utils.loadInputAsStrings("2024/day7.txt"))

    println(s"Part 1: ${totalCalibrationResultTwoOps(equations)}")
    println(s"Part 2: ${totalCalibrationResultThreeOps(equations)}")
  }

  final case class Equation(operands: Seq[Long], result: Long)
  type Operator = (Long, Long) => Long

  def totalCalibrationResultTwoOps(equations: Seq[Equation]): Long = {
    val operators = Seq[Operator](_ + _, _ * _)
    equations.collect { case eq if canBeMadeTrue(eq, operators) => eq.result }.sum
  }

  def totalCalibrationResultThreeOps(equations: Seq[Equation]): Long = {
    val operators = Seq[Operator](_ + _, _ * _, (a, b) => s"$a$b".toLong)
    equations.collect { case eq if canBeMadeTrue(eq, operators) => eq.result }.sum
  }

  private def canBeMadeTrue(equation: Equation, operators: Seq[Operator]) = {
    def dfs(acc: Long, operands: Seq[Long]): Boolean =
      if (operands.isEmpty) acc == equation.result
      else operators.exists(op => dfs(op(acc, operands.head), operands.tail))

    dfs(acc = equation.operands.head, operands = equation.operands.tail)
  }

  private def parseInput(lines: Seq[String]) =
    lines.map { case s"$result: $operands" =>
      Equation(operands.split(' ').map(_.toLong), result.toLong)
    }

}
