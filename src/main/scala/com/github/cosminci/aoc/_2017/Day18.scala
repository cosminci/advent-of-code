package com.github.cosminci.aoc._2017

import com.github.cosminci.aoc.utils

import scala.util.Try

object Day18 {

  def main(args: Array[String]): Unit = {
    val ops = parseInput(utils.loadInputAsStrings("2017/day18.txt"))

    println(s"Part 1: ${firstFrequencyRecvd(ops)}")
    println(s"Part 2: ${timesProgram1Sent(ops)}")
  }

  sealed trait Op
  final case class Snd(a: Value)              extends Op
  final case class Set(a: Register, b: Value) extends Op
  final case class Add(a: Register, b: Value) extends Op
  final case class Mul(a: Register, b: Value) extends Op
  final case class Mod(a: Register, b: Value) extends Op
  final case class Rcv(a: Register)           extends Op
  final case class Jgz(a: Value, b: Value)    extends Op

  sealed trait Value
  final case class Register(name: Char)  extends Value
  final case class Constant(value: Long) extends Value

  def firstFrequencyRecvd(ops: Seq[Op]): Long = {
    @annotation.tailrec
    def dfs(i: Long, r: Map[Char, Long], lastFreq: Long): Long =
      if (i < 0 || i >= ops.size) lastFreq
      else
        ops(i.toInt) match {
          case Snd(a)    => dfs(i + 1, r, unwrap(r, a))
          case Set(a, b) => dfs(i + 1, r.updated(a.name, unwrap(r, b)), lastFreq)
          case Add(a, b) => dfs(i + 1, r.updated(a.name, unwrap(r, a) + unwrap(r, b)), lastFreq)
          case Mul(a, b) => dfs(i + 1, r.updated(a.name, unwrap(r, a) * unwrap(r, b)), lastFreq)
          case Mod(a, b) => dfs(i + 1, r.updated(a.name, unwrap(r, a) % unwrap(r, b)), lastFreq)
          case Rcv(a)    => if (unwrap(r, a) != 0) lastFreq else dfs(i + 1, r, lastFreq)
          case Jgz(a, b) => if (unwrap(r, a) > 0) dfs(i + unwrap(r, b), r, lastFreq) else dfs(i + 1, r, lastFreq)
        }

    dfs(i = 0, r = Map.empty.withDefaultValue(0), lastFreq = -1)
  }

  final case class State(i: Int, r: Map[Char, Long], msg: Seq[Long], blocked: Boolean, sentCnt: Int)

  def timesProgram1Sent(ops: Seq[Op]): Int = {
    @annotation.tailrec
    def dfs(i: Int, p1: State, p2: State): Int = {
      ops(p1.i) match {
        case Set(Register(a), b) =>
          dfs(i + 1, p2, p1.copy(i = p1.i + 1, r = p1.r.updated(a, unwrap(p1.r, b))))
        case Add(Register(a), b) =>
          dfs(i + 1, p2, p1.copy(i = p1.i + 1, r = p1.r.updated(a, p1.r(a) + unwrap(p1.r, b))))
        case Mul(Register(a), b) =>
          dfs(i + 1, p2, p1.copy(i = p1.i + 1, r = p1.r.updated(a, p1.r(a) * unwrap(p1.r, b))))
        case Mod(Register(a), b) =>
          dfs(i + 1, p2, p1.copy(i = p1.i + 1, r = p1.r.updated(a, p1.r(a) % unwrap(p1.r, b))))
        case Jgz(a, b) =>
          dfs(i + 1, p2, p1.copy(i = p1.i + (if (unwrap(p1.r, a) > 0) unwrap(p1.r, b).toInt else 1)))
        case Snd(a) =>
          val (p1Updated, p2Updated) = send(p1, p2, a)
          dfs(i + 1, p2Updated, p1Updated)
        case Rcv(Register(a)) =>
          p1.msg match {
            case v +: msgs =>
              dfs(i + 1, p2, p1.copy(i = p1.i + 1, r = p1.r.updated(a, v), msg = msgs, blocked = false))
            case Seq() =>
              if (p2.blocked) if (i % 2 == 0) p2.sentCnt else p1.sentCnt
              else dfs(i, p2, p1.copy(blocked = true))
          }
      }
    }
    val program0 = State(i = 0, r = Map('p' -> 0L).withDefaultValue(0L), msg = Seq.empty, blocked = false, sentCnt = 0)
    val program1 = State(i = 0, r = Map('p' -> 1L).withDefaultValue(0L), msg = Seq.empty, blocked = false, sentCnt = 0)
    dfs(i = 0, program0, program1)
  }

  private def send(s1: State, s2: State, a: Value) = (
    s1.copy(i = s1.i + 1, sentCnt = s1.sentCnt + 1),
    s2.copy(msg = s2.msg :+ unwrap(s1.r, a))
  )

  private def unwrap(regs: Map[Char, Long], r: Value) = r match {
    case Register(name)  => regs(name)
    case Constant(value) => value
  }

  private def parseInput(input: Seq[String]) =
    input.map {
      case s"snd $a"    => Snd(parseValue(a))
      case s"set $a $b" => Set(Register(a.head), parseValue(b))
      case s"add $a $b" => Add(Register(a.head), parseValue(b))
      case s"mul $a $b" => Mul(Register(a.head), parseValue(b))
      case s"mod $a $b" => Mod(Register(a.head), parseValue(b))
      case s"rcv $a"    => Rcv(Register(a.head))
      case s"jgz $a $b" => Jgz(parseValue(a), parseValue(b))
    }

  private def parseValue(s: String) =
    Try(s.toLong).map(Constant).getOrElse(Register(s.head))

}
