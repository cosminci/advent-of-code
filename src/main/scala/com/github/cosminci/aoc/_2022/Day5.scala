package com.github.cosminci.aoc._2022

import com.github.cosminci.aoc.utils

object Day5 {

  def main(args: Array[String]): Unit = {
    val inputState = utils.loadInputAsStrings("2022/day5_state.txt")
    val inputMoves = utils.loadInputAsStrings("2022/day5_moves.txt")

    val (state, moves) = (parseInputState(inputState), parseInputMoves(inputMoves))

    println(s"Part 1: ${stackHeadsAfterSingleCrateMoves(state, moves)}")
    println(s"Part 2: ${stackHeadsAfterBulkCrateMoves(state, moves)}")
  }

  def stackHeadsAfterSingleCrateMoves(state: Seq[Seq[Char]], moves: Seq[(Int, Int, Int)]): String =
    moves
      .foldLeft(state) { case (stacks, (from, to, count)) => move(stacks, from, to, count, bulk = false) }
      .map(_.head)
      .mkString

  def stackHeadsAfterBulkCrateMoves(state: Seq[Seq[Char]], moves: Seq[(Int, Int, Int)]): String =
    moves
      .foldLeft(state) { case (stacks, (from, to, count)) => move(stacks, from, to, count, bulk = true) }
      .map(_.head)
      .mkString

  private def move(stacks: Seq[Seq[Char]], from: Int, to: Int, count: Int, bulk: Boolean) = {
    val (popped, remaining) = stacks(from - 1).splitAt(count)
    val toPush              = if (bulk) popped else popped.reverse

    stacks.updated(from - 1, remaining).updated(to - 1, toPush ++ stacks(to - 1))
  }

  private def parseInputState(input: Seq[String]) =
    input
      .map(line => line.grouped(4).map(el => el(1)).toSeq)
      .transpose
      .map(stack => stack.filter(_ != ' '))

  private def parseInputMoves(input: Seq[String]) = {
    val pattern = "move ([0-9]+) from ([0-9]+) to ([0-9]+)".r
    input.map { case pattern(count, from, to) => (from.toInt, to.toInt, count.toInt) }
  }
}
