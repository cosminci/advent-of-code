package com.github.cosminci.aoc._2017

import com.github.cosminci.aoc.utils

import scala.util.Try

object Day23 {

  def main(args: Array[String]): Unit = {
    val ops = parseInput(utils.loadInputAsStrings("2017/day23.txt"))

    println(s"Part 1: ${mulOpCount(ops)}")
    println(s"Part 2: ${finalHValue(ops)}")
  }

  sealed trait Value
  final case class Register(name: Char)  extends Value
  final case class Constant(value: Long) extends Value

  sealed trait Op
  final case class Set(a: Register, b: Value) extends Op
  final case class Sub(a: Register, b: Value) extends Op
  final case class Mul(a: Register, b: Value) extends Op
  final case class Jnz(a: Value, b: Int)      extends Op

  def mulOpCount(ops: Seq[Op]): Int = {
    @annotation.tailrec
    def dfs(i: Int, r: Map[Char, Long], mulCnt: Int): Int =
      if (i >= ops.length) mulCnt
      else ops(i) match {
        case Set(Register(a), b) => dfs(i + 1, r.updated(a, unwrap(r, b)), mulCnt)
        case Sub(Register(a), b) => dfs(i + 1, r.updated(a, r(a) - unwrap(r, b)), mulCnt)
        case Mul(Register(a), b) => dfs(i + 1, r.updated(a, r(a) * unwrap(r, b)), mulCnt + 1)
        case Jnz(a, b)           => if (unwrap(r, a) != 0) dfs(i + b, r, mulCnt) else dfs(i + 1, r, mulCnt)
      }

    dfs(i = 0, r = ('a' to 'h').map(_ -> 0L).toMap, mulCnt = 0)
  }

  private def finalHValue(ops: Seq[Op]): Int = {
    val bSeed = ops.collectFirst { case Set(Register('b'), Constant(b)) => b }.get
    val b     = bSeed * 100 + 100_000
    (b to b + 17000 by 17).foldLeft(0) { (h, i) =>
      if ((2 to math.sqrt(i).toInt).exists(j => i % j == 0)) h + 1 else h
    }
  }

  private def unwrap(regs: Map[Char, Long], r: Value) = r match {
    case Register(name)  => regs(name)
    case Constant(value) => value
  }

  private def parseInput(input: Seq[String]) = input.map {
    case s"set $a $b" => Set(Register(a.head), parseValue(b))
    case s"sub $a $b" => Sub(Register(a.head), parseValue(b))
    case s"mul $a $b" => Mul(Register(a.head), parseValue(b))
    case s"jnz $a $b" => Jnz(parseValue(a), b.toInt)
  }

  private def parseValue(s: String) =
    Try(s.toLong).map(Constant).getOrElse(Register(s.head))

}
