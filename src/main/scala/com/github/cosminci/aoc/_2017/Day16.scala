package com.github.cosminci.aoc._2017

import com.github.cosminci.aoc.utils

import scala.util.chaining._

object Day16 {

  def main(args: Array[String]): Unit = {
    val moves    = utils.loadInputAsStrings("2017/day16.txt").head.split(',')
    val programs = ('a' to 'p').mkString

    println(s"Part 1: ${updatePrograms(programs, moves, iters = 1)}")
    println(s"Part 2: ${updatePrograms(programs, moves, iters = 1_000_000_000)}")
  }

  def updatePrograms(programs: String, moves: Array[String], iters: Int): String = {
    @annotation.tailrec
    def dfs(programs: String, memory: Map[String, Int], i: Int): String =
      if (i == 0) programs
      else {
        val newPrograms = moves.foldLeft(programs)(updatePrograms)
        memory.get(newPrograms) match {
          case None =>
            dfs(newPrograms, memory.updated(newPrograms, i - 1), i - 1)
          case Some(j) =>
            val cycleLength = j - i + 1
            val newIters    = i - (i / cycleLength * cycleLength)
            Iterator.iterate(programs)(p => moves.foldLeft(p)(updatePrograms)).drop(newIters).next()
        }
      }

    dfs(programs, memory = Map.empty, iters)
  }

  private def updatePrograms(programs: String, move: String) =
    move match {
      case s"s$n"    => spin(programs, n.toInt)
      case s"x$i/$j" => exchange(programs, i.toInt, j.toInt)
      case s"p$a/$b" => partner(programs, a.head, b.head)
    }

  private def spin(s: String, n: Int) =
    s.splitAt(s.length - n).pipe { case (fh, sh) => sh ++ fh }

  private def exchange(programs: String, i: Int, j: Int) =
    programs.updated(i, programs(j)).updated(j, programs(i))

  private def partner(programs: String, a: Char, b: Char) =
    exchange(programs, programs.indexOf(a), programs.indexOf(b))

}
