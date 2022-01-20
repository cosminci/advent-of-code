package com.github.cosminci.aoc._2020

import com.github.cosminci.aoc.utils

object Day19 {

  def main(args: Array[String]): Unit = {
    val input                    = utils.loadInputAsStrings("2020/day19.txt")
    val (rules, chars, messages) = parseInput(input)

    println(s"Part 1: ${countValidMessages(rules, chars, messages)}")
    println(s"Part 2: ${countValidMessagesWithRuleCycles(rules, chars, messages)}")
  }

  type Rule = Seq[Seq[Int]]

  def countValidMessages(rules: Map[Int, Rule], chars: Seq[Int], messages: Seq[String]): Int =
    messages.count(stripRule(rules, chars, id = 0, _).contains(""))

  def countValidMessagesWithRuleCycles(rules: Map[Int, Rule], chars: Seq[Int], messages: Seq[String]): Int = {
    val ruleOverrides = Map(
      8  -> Seq(Seq(42, 8), Seq(42)),
      11 -> Seq(Seq(42, 11, 31), Seq(42, 31))
    )
    countValidMessages(rules ++ ruleOverrides, chars, messages)
  }

  private def stripRule(rules: Map[Int, Rule], chars: Seq[Int], id: Int, s: String): Seq[String] =
    if (s.isEmpty) Seq.empty
    else if (chars.contains(id)) Option.when(s.head == chars.indexOf(id) + 'a')(Seq(s.tail)).getOrElse(Seq.empty)
    else rules(id).flatMap(stripRuleSeq(rules, chars, _, s))

  private def stripRuleSeq(rules: Map[Int, Rule], chars: Seq[Int], seq: Seq[Int], s: String): Seq[String] =
    if (seq.isEmpty) Seq(s)
    else stripRule(rules, chars, seq.head, s).flatMap(stripRuleSeq(rules, chars, seq.tail, _))

  private val rulePattern = "([0-9]+): (.*)$".r
  private val orPattern   = "(.*) \\| (.*)".r

  private def parseInput(input: Seq[String]) = {
    val (rules, chars) = input
      .takeWhile(_.nonEmpty)
      .foldLeft(Map.empty[Int, Rule], Seq.fill(2)(0)) { case ((rules, chars), line) =>
        line match {
          case rulePattern(id, "\"a\"")           => (rules, chars.updated(0, id.toInt))
          case rulePattern(id, "\"b\"")           => (rules, chars.updated(1, id.toInt))
          case rulePattern(id, orPattern(s1, s2)) => (rules.updated(id.toInt, Seq(s1, s2).map(parseSeq)), chars)
          case rulePattern(id, s)                 => (rules.updated(id.toInt, Seq(parseSeq(s))), chars)
        }
      }

    val messages = input.slice(rules.size + chars.length + 1, input.length)
    (rules, chars, messages)
  }

  private def parseSeq(s: String) = s.trim.split(" ").toSeq.map(_.toInt)
}
