package com.github.cosminci.aoc._2018

import com.github.cosminci.aoc.utils

import scala.util.chaining._

object Day12 {

  def main(args: Array[String]): Unit = {
    val (potState, notes) = parseInput(utils.loadInputAsStrings("2018/day12.txt"))

    println(s"Part 1: ${sumOfPotNumbersWithPlants(potState, notes, totalGens = 20)}")
    println(s"Part 2: ${sumOfPotNumbersWithPlants(potState, notes, totalGens = 50_000_000_000L)}")
  }

  final case class PotState(pots: String, offset: Long)

  def sumOfPotNumbersWithPlants(state: PotState, notes: Map[String, Char], totalGens: Long): Long = {
    val PotState(pots, offset) = passTime(state, notes, totalGens)
    pots.zipWithIndex.collect { case ('#', i) => i + offset }.sum
  }

  private def passTime(state: PotState, notes: Map[String, Char], totalGens: Long): PotState = {
    @annotation.tailrec
    def dfs(gen: Int, state: PotState): PotState =
      if (gen == totalGens) state
      else {
        val nextState = passTime(state, notes)
        if (nextState.pots != state.pots) dfs(gen + 1, nextState)
        else PotState(nextState.pots, state.offset + (nextState.offset - state.offset) * (totalGens - gen))
      }

    dfs(gen = 0, state)
  }

  private def passTime(state: PotState, notes: Map[String, Char]) = {
    val newPots       = s"....${state.pots}....".sliding(5).map(notes.getOrElse(_, '.')).mkString
    val (first, last) = (newPots.indexOf('#'), newPots.lastIndexOf('#'))
    PotState(newPots.slice(first, last + 1), state.offset + first - 2)
  }

  private def parseInput(input: Seq[String]) = {
    val stateRow = input.head.pipe { case s"initial state: $row" => row }
    val pots     = stateRow.dropWhile(_ == '.')
    val notes    = input.drop(2).collect { case s"$pattern => $result" => pattern -> result.head }.toMap
    (PotState(pots, stateRow.indexOf('#')), notes)
  }

}
