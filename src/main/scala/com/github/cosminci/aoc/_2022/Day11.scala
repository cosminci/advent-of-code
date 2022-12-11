package com.github.cosminci.aoc._2022

import com.github.cosminci.aoc.utils
import monocle.syntax.all._

import scala.util.chaining._

object Day11 {

  def main(args: Array[String]): Unit = {
    val input = utils.loadInputAsStrings("2022/day11.txt")

    val initialState = parseInput(input)
    val modProduct   = initialState.map(_.mod).product

    println(s"Part 1: ${monkeyBusiness(initialState, decreaseWorry = _ / 3, rounds = 20)}")
    println(s"Part 2: ${monkeyBusiness(initialState, decreaseWorry = _ % modProduct, rounds = 10000)}")
  }

  def monkeyBusiness(initialState: Seq[Monkey], decreaseWorry: Long => Long, rounds: Int): Long =
    Iterator
      .iterate(initialState)(playRound(_, decreaseWorry))
      .drop(rounds).next()
      .map(_.throwCount).sorted.takeRight(2)
      .product

  private def playRound(state: Seq[Monkey], decreaseWorry: Long => Long) = {
    def updateState(state: Seq[Monkey], currMonkey: Int) = {
      val Monkey(op, mod, (nextWhenModPass, nextWhenModFail), items, _) = state(currMonkey)

      val newState = items
        .map(increaseWorry(_, op).pipe(decreaseWorry))
        .foldLeft(state) { (state, worryValue) =>
          val nextMonkey = if (worryValue % mod == 0) nextWhenModPass else nextWhenModFail
          state.updated(nextMonkey, state(nextMonkey).focus(_.items).modify(_ :+ worryValue))
        }

      newState.updated(currMonkey,
        newState(currMonkey)
          .focus(_.items).modify(_ => Seq.empty)
          .focus(_.throwCount).modify(_ + items.length)
      )
    }

    state.indices.foldLeft(state)(updateState)
  }

  private def increaseWorry(value: Long, op: Operation): Long = op match {
    case MultiplyBySelf     => value * value
    case MultiplyBy(factor) => value * factor
    case IncreaseBy(factor) => value + factor
  }

  case class Monkey(op: Operation, mod: Int, next: (Int, Int), items: Seq[Long], throwCount: Long)

  sealed trait Operation
  final case object MultiplyBySelf         extends Operation
  final case class MultiplyBy(factor: Int) extends Operation
  final case class IncreaseBy(factor: Int) extends Operation

  private def parseInput(input: Seq[String]): Seq[Monkey] =
    input.grouped(7).map(parseMonkey).toSeq

  private def parseMonkey(monkeyStrs: Seq[String]) = {
    val items = monkeyStrs(1).substring(18).split(", ").map(_.toLong)
    val op    = parseOp(monkeyStrs(2))
    val mod   = monkeyStrs(3).reverse.takeWhile(_.isDigit).reverse.toInt
    val next  = (monkeyStrs(4).last - '0', monkeyStrs(5).last - '0')
    Monkey(op, mod, next, items, throwCount = 0)
  }

  private def parseOp(s: String) =
    s.substring(19) match {
      case s"old * old"     => MultiplyBySelf
      case s"old * $factor" => MultiplyBy(factor.toInt)
      case s"old + $factor" => IncreaseBy(factor.toInt)
    }

}
