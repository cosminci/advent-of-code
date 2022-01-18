package com.github.cosminci.aoc._2020

import com.github.cosminci.aoc.utils

object Day16 {

  def main(args: Array[String]): Unit = {
    val inputData = utils.loadInputAsStrings("2020/day16.txt")

    val (rules, myTicket, otherTickets) = parseInput(inputData)

    println(s"Part 1: ${ticketScanningErrorRate(rules, otherTickets)}")
    println(s"Part 2: ${departuresValuesProduct(rules, myTicket, otherTickets)}")
  }

  def ticketScanningErrorRate(rules: Seq[Rule], otherTickets: Seq[Ticket]): Int = {
    val intervals = mergeIntervals(rules.flatMap(r => Seq(r.intervalA, r.intervalB)).sortBy(_.min))
    otherTickets.flatMap(_.filter(v => intervals.forall(i => v < i.min || v > i.max))).sum
  }

  private def mergeIntervals(intervals: Seq[Interval]): Seq[Interval] =
    intervals.tail.foldLeft(Seq(intervals.head)) { case (prev, curr) =>
      if (prev.last.max <= curr.min) prev.dropRight(1) :+ Interval(prev.last.min, curr.max)
      else prev :+ curr
    }

  private def departuresValuesProduct(rules: Seq[Rule], myTicket: Ticket, tickets: Seq[Ticket]): Long = {
    val intervals    = mergeIntervals(rules.flatMap(r => Seq(r.intervalA, r.intervalB)).sortBy(_.min))
    val validTickets = tickets.filter(_.forall(v => intervals.exists(i => v >= i.min && v <= i.max)))

    def ruleMatches(r: Rule, idx: Int) =
      validTickets.map(_(idx)).forall { v =>
        Seq(r.intervalA, r.intervalB).exists(i => v >= i.min && v <= i.max)
      }

    tickets.head.indices
      .map { idx => (idx, rules.filter(ruleMatches(_, idx))) }
      .sortBy { case (_, matchingRules) => matchingRules.length }
      .foldLeft(Map.empty[String, Int]) { case (assignedRules, (idx, matchingRules)) =>
        assignedRules ++ matchingRules.collectFirst {
          case rule if !assignedRules.contains(rule.field) => (rule.field, idx)
        }
      }
      .collect { case (field, idx) if field.startsWith("departure") => myTicket(idx).toLong }
      .product
  }

  type Ticket = Array[Int]
  case class Rule(field: String, intervalA: Interval, intervalB: Interval)
  case class Interval(min: Int, max: Int)

  private val rulePattern = "([a-z ]+): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)".r

  private def parseInput(input: Seq[String]) = {
    val rules = input.takeWhile(_ != "").map { case rulePattern(field, min1, max1, min2, max2) =>
      Rule(field, Interval(min1.toInt, max1.toInt), Interval(min2.toInt, max2.toInt))
    }

    val tickets      = input(rules.length + 2) +: input.slice(rules.length + 5, input.length)
    val head +: tail = tickets.map(_.split(",").map(_.toInt))

    (rules, head, tail)
  }
}
