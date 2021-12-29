package com.github.cosminci.aoc._2020

import com.github.cosminci.aoc.utils

import scala.annotation.tailrec

object Day8 {

  def main(args: Array[String]): Unit = {
    val ops = utils.loadInputAsStrings("2020/day8.txt").map(parseInput)

    println(s"Part 1: ${lastValueBeforeCycle(ops)}")
    println(s"Part 1: ${removeLoopAndComputeResult(ops)}")
  }

  def lastValueBeforeCycle(ops: Seq[(String, Int)]): Int =
    execute(acc = 0, currOp = 0, visited = Set.empty, ops)._1

  def removeLoopAndComputeResult(ops: Seq[(String, Int)]): Int =
    ops.indices.view
      .filter(i => ops(i)._1 == "nop" || ops(i)._1 == "jmp")
      .map(i => ops.updated(i, ops(i).copy(_1 = Option.when(ops(i)._1 == "jmp")("nop").getOrElse("jmp"))))
      .map(ops => execute(acc = 0, currOp = 0, visited = Set.empty, ops))
      .collect { case (result, successful) if successful => result }
      .head

  @tailrec
  private def execute(acc: Int, currOp: Int, visited: Set[Int], ops: Seq[(String, Int)]): (Int, Boolean) =
    if (visited.contains(currOp)) (acc, false)
    else if (currOp == ops.size) (acc, true)
    else {
      val (newAcc, nextOp) = ops(currOp) match {
        case ("nop", _)     => (acc, currOp + 1)
        case ("acc", value) => (acc + value, currOp + 1)
        case (_, value)     => (acc, currOp + value)
      }
      execute(newAcc, nextOp, visited + currOp, ops)
    }

  private def parseInput(line: String): (String, Int) =
    line.split(' ') match { case Array(kind, value) => (kind, value.toInt) }
}
