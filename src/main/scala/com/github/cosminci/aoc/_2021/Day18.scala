package com.github.cosminci.aoc._2021

import com.github.cosminci.aoc.utils

object Day18 {

  def main(args: Array[String]): Unit = {
    val nums = utils.loadInputAsStrings("2021/day18.txt").map(s => parseNumber(s)._1)

    println(s"Part I: ${magnitude(nums.reduce(add))}")
    println(s"Part II: ${nums.combinations(2).flatMap(s => Seq(s, s.reverse).map(s => magnitude(s.reduce(add)))).max}")
  }

  sealed trait Number
  case class Literal(value: Int)               extends Number
  case class Pair(left: Number, right: Number) extends Number

  private def parseNumber(s: String): (Number, String) = s.head match {
    case '[' =>
      val (left, afterLeft)   = parseNumber(s.drop(1))
      val (right, afterRight) = parseNumber(afterLeft.drop(1))
      (Pair(left, right), afterRight.drop(1))
    case _ =>
      (Literal(s.head - '0'), s.drop(1))
  }

  private def magnitude(n: Number): Int = n match {
    case Literal(value)    => value
    case Pair(left, right) => 3 * magnitude(left) + 2 * magnitude(right)
  }

  private def add(n1: Number, n2: Number): Number = reduce(Pair(n1, n2))

  private def reduce(n: Number): Number =
    explode(n).orElse(split(n)).map(reduce).getOrElse(n)

  private def explode(n: Number): Option[Number] = {
    def dfs(n: Number, depth: Int): Option[(Number, Int, Int)] = n match {
      case _: Literal => None
      case Pair(Literal(left), Literal(right)) =>
        Option.when(depth >= 4)(Literal(0), left, right)
      case Pair(left, right) =>
        dfs(left, depth + 1).map { case (newLeft, leftCarry, rightCarry) =>
          (Pair(newLeft, addLeft(right, rightCarry)), leftCarry, 0)
        }
        .orElse(dfs(right, depth + 1).map { case (newRight, leftCarry, rightCarry) =>
          (Pair(addRight(left, leftCarry), newRight), 0, rightCarry)
        })
    }
    dfs(n, depth = 0).map(_._1)
  }

  private def addLeft(n: Number, toAdd: Int): Number = n match {
    case Literal(value) => Literal(value + toAdd)
    case Pair(a, b)     => Pair(addLeft(a, toAdd), b)
  }

  private def addRight(n: Number, toAdd: Int): Number = n match {
    case Literal(value) => Literal(value + toAdd)
    case Pair(a, b)     => Pair(a, addRight(b, toAdd))
  }

  private def split(n: Number): Option[Number] = n match {
    case Literal(value) =>
      Option.when(value >= 10)(Pair(Literal(value / 2), Literal(value - value / 2)))
    case Pair(left, right) =>
      split(left).map(Pair(_, right)).orElse(split(right).map(Pair(left, _)))
  }
}
