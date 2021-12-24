package com.github.cosminci.aoc._2021

import com.github.cosminci.aoc.utils

object Day24 {
  def main(args: Array[String]): Unit = {
    val steps = parseSteps(utils.loadInputAsStrings("2021/day24.txt"))
    val rules = discoverRules(steps)

    println(s"Part I: ${maximize(rules).mkString}")
    println(s"Part II: ${minimize(rules).mkString}")
  }

  sealed trait Step
  case class Push(add: Int)   extends Step
  case class Pop(remove: Int) extends Step

  case class Rule(digit1: Int, digit2: Int, diff: Int)

  def maximize(rules: Seq[Rule]): Seq[Int] =
    rules.foldLeft(Seq.fill[Int](rules.length * 2)(0)) { case (digits, Rule(d1, d2, diff)) =>
      digits.updated(d1, 9).updated(d2, 9 - diff)
    }

  def minimize(rules: Seq[Rule]): Seq[Int] =
    rules.foldLeft(Seq.fill[Int](rules.length * 2)(0)) { case (digits, Rule(d1, d2, diff)) =>
      digits.updated(d2, 1).updated(d1, 1 + diff)
    }

  private def discoverRules(steps: Seq[Step]): Seq[Rule] =
    steps.zipWithIndex
      .foldLeft(Seq.empty[(Int, Int)], Seq.empty[Rule]) { case ((stack, rules), (step, d)) =>
        step match {
          case Push(add) => (stack :+ ((d, add)), rules)
          case Pop(remove) =>
            val (d1, add) = stack.last
            val diff      = add + remove
            val rule      = Option.when(diff >= 0)(Rule(d, d1, diff)).getOrElse(Rule(d1, d, -diff))
            (stack.dropRight(1), rules :+ rule)
        }
      }._2

  private def parseSteps(lines: Seq[String]): Seq[Step] = {
    val parseOp   = (line: String) => line.split(" ").last.toInt
    val parseStep = (step: Seq[String]) => if (parseOp(step(4)) == 1) Push(parseOp(step(15))) else Pop(parseOp(step(5)))
    lines.grouped(18).map(parseStep).toSeq
  }
}
