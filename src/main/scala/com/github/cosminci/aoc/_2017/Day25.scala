package com.github.cosminci.aoc._2017

import com.github.cosminci.aoc.utils

object Day25 {

  def main(args: Array[String]): Unit = {
    val (startState, blueprints, steps) = parseInput(utils.loadInputAsStrings("2017/day25.txt"))

    println(s"Part 1: ${diagnosticChecksum(startState, blueprints, steps)}")
  }

  final case class StateActions(when0: Action, when1: Action)
  final case class Action(write: Int, move: Int, nextState: Char)

  def diagnosticChecksum(startState: Char, blueprints: Map[Char, StateActions], steps: Int): Int = {
    @annotation.tailrec
    def dfs(i: Int, pos: Int, state: Char, tape: Map[Int, Int]): Int =
      if (i == steps) tape.values.sum
      else {
        val action = if (tape.getOrElse(pos, 0) == 0) blueprints(state).when0 else blueprints(state).when1
        dfs(i + 1, pos + action.move, action.nextState, tape.updated(pos, action.write))
      }

    dfs(i = 0, pos = 0, startState, tape = Map.empty)
  }

  private def parseInput(input: Seq[String]): (Char, Map[Char, StateActions], Int) = {
    val startState = input.head match { case s"Begin in state $s." => s.head }
    val steps      = input(1) match { case s"$_ after $steps steps." => steps.toInt }
    val states     = input.drop(3).grouped(10).map(parseState).toMap
    (startState, states, steps)
  }

  private def parseState(lines: Seq[String]) = {
    val state = lines.head match { case s"In state $s:" => s.head }
    val when0 = parseAction(lines.slice(2, 5))
    val when1 = parseAction(lines.slice(6, 9))
    (state, StateActions(when0, when1))
  }

  private def parseAction(lines: Seq[String]) = {
    val write     = lines.head match { case s"$_ Write the value $v." => v.toInt }
    val move      = lines(1) match { case s"$_ Move one slot to the $d." => if (d == "right") 1 else -1 }
    val nextState = lines(2) match { case s"$_ Continue with state $s." => s.head }
    Action(write, move, nextState)
  }

}
