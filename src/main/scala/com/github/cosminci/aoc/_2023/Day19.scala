package com.github.cosminci.aoc._2023

import com.github.cosminci.aoc.utils

import scala.util.chaining._

object Day19 {

  def main(args: Array[String]): Unit = {
    val input = utils.loadInputAsStrings("2023/day19.txt")

    val (workflows, parts) = parseInput(input)

    println(s"Part 1: ${acceptedPartRatingsSum(workflows, parts)}")
    println(s"Part 2: ${distinctRatingCombinations(workflows)}")
  }

  final case class Step(category: Char, range: Range, targetWf: String)
  final case class Workflow(steps: Seq[Step], fallback: String)

  def acceptedPartRatingsSum(workflows: Map[String, Workflow], parts: Seq[Map[Char, Int]]): Int =
    parts.filter(isAccepted(_, workflows)).flatMap(_.values).sum

  def distinctRatingCombinations(workflows: Map[String, Workflow]): Long = {
    def continueToWorkflow(ranges: Map[Char, Range], targetWf: String) =
      if (targetWf == "R") 0L
      else if (targetWf == "A") ranges.values.map(_.length.toLong).product
      else count(ranges, workflows(targetWf))

    def count(ranges: Map[Char, Range], wf: Workflow): Long = {
      val (count, unmatchedRanges) = wf.steps.foldLeft(0L, ranges) { case ((count, ranges), step) =>
        val (incl, excl) = splitRange(ranges(step.category), step.range)
        val inclRanges   = ranges - step.category + (step.category -> incl)
        val exclRanges   = ranges - step.category + (step.category -> excl)
        (count + continueToWorkflow(inclRanges, step.targetWf), exclRanges)
      }
      count + continueToWorkflow(unmatchedRanges, wf.fallback)
    }

    count(ranges = Seq('x', 'm', 'a', 's').map(_ -> (1 to 4000)).toMap, workflows("in"))
  }

  private def splitRange(r1: Range, r2: Range): (Range, Range) = {
    val (s1, e1, s2, e2) = (r1.head, r1.last, r2.head, r2.last)
    if (s2 == 1) (s1 to e1.min(e2), e1.min(e2) + 1 to e1)
    else (s1.max(s2) to e1, s1 to e1.min(s2 - 1))
  }

  private def isAccepted(part: Map[Char, Int], workflows: Map[String, Workflow]): Boolean = {
    @annotation.tailrec
    def dfs(wf: Workflow): Boolean = {
      val nextWf = wf.steps
        .collectFirst { case step if step.range.contains(part(step.category)) => step.targetWf }
        .getOrElse(wf.fallback)

      nextWf != "R" && (nextWf == "A" || dfs(workflows(nextWf)))
    }

    dfs(workflows("in"))
  }

  private def parseInput(input: Seq[String]) = {
    val (workflowLines, partLines) = input.splitAt(input.indexWhere(_.isEmpty))
    (workflowLines.map(parseWorkflow).toMap, partLines.tail.map(parsePart))
  }

  private def parsePart(s: String) = s match {
    case s"{x=$x,m=$m,a=$a,s=$s}" => Map('x' -> x.toInt, 'm' -> m.toInt, 'a' -> a.toInt, 's' -> s.toInt)
  }

  private def parseWorkflow(s: String) = s match {
    case s"$name{$stepsStr}" =>
      val (stepStrings, fallback) = stepsStr.split(',').pipe(lst => lst.splitAt(lst.length - 1))
      val steps = stepStrings.map {
        case s"$cat>$value:$target" => Step(cat.head, value.toInt + 1 to 4000, target)
        case s"$cat<$value:$target" => Step(cat.head, 1 until value.toInt, target)
      }
      name -> Workflow(steps, fallback.head)
  }

}
