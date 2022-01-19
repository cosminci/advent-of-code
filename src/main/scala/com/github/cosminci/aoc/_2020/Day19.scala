package com.github.cosminci.aoc._2020

import com.github.cosminci.aoc.utils

object Day19 {

  def main(args: Array[String]): Unit = {
    val input             = utils.loadInputAsStrings("2020/day19.txt")
    val (rules, messages) = parseInput(input)

    println(s"Part 1: ${countValidMessages(rules, messages)}")
    println(s"Part 1: ${countValidMessagesWithRouteCycles(rules, messages)}")
  }

  def countValidMessages(rules: Map[Int, Rule], messages: Seq[String]): Int =
    messages.count(s => validMatches(rules, rules(0), s).exists(_.remainingStr == ""))

  def countValidMessagesWithRouteCycles(rules: Map[Int, Rule], messages: Seq[String]): Int =
    countValidMessages(
      rules ++ Map(
        8  -> Or(Sequence(Seq(42, 8)), Sequence(Seq(42))),
        11 -> Or(Sequence(Seq(42, 11, 31)), Sequence(Seq(42, 31)))
      ),
      messages
    )

  private def validMatches(rules: Map[Int, Rule], rule: Rule, s: String): Seq[Matched] =
    if (s.isEmpty) Seq.empty
    else
      rule match {
        case LetterRule(letter) => Seq.empty ++ Option.when(s.head == letter)(Matched(s.tail))
        case seq: Sequence      => matchesSequence(rules, seq, s)
        case Or(rules1, rules2) => matchesSequence(rules, rules1, s) ++ matchesSequence(rules, rules2, s)
      }

  private def matchesSequence(rules: Map[Int, Rule], seq: Sequence, s: String): Seq[Matched] =
    seq.rules.tail.foldLeft(validMatches(rules, rules(seq.rules.head), s)) { (acc, ruleId) =>
      acc.flatMap { prevMatch =>
        validMatches(rules, rules(ruleId), prevMatch.remainingStr)
      }
    }

  sealed trait Rule
  case class LetterRule(letter: Char)       extends Rule
  case class Or(s1: Sequence, s2: Sequence) extends Rule
  case class Sequence(rules: Seq[Int])      extends Rule

  case class Matched(remainingStr: String)

  private val ruleIdPattern = "([0-9]+): (.*)$".r
  private val charPattern   = "\"([a-z])\"".r
  private val orPattern     = "(.*) \\| (.*)".r

  private def parseInput(input: Seq[String]) = {
    val rules = input
      .takeWhile(_.nonEmpty)
      .map {
        case ruleIdPattern(id, charPattern(letter))       => id.toInt -> LetterRule(letter.charAt(0))
        case ruleIdPattern(id, orPattern(rules1, rules2)) => id.toInt -> Or(parseSeq(rules1), parseSeq(rules2))
        case ruleIdPattern(id, ruleSequence)              => id.toInt -> parseSeq(ruleSequence)
      }
      .toMap

    val messages = input.slice(rules.size + 1, input.length)
    (rules, messages)
  }

  private def parseSeq(s: String): Sequence = Sequence(s.trim.split(" ").map(_.toInt))
}
