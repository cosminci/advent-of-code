package com.github.cosminci.aoc._2025

import com.github.cosminci.aoc.utils
import com.microsoft.z3._

import scala.util.Using
import scala.util.chaining.scalaUtilChainingOps

object Day10 {

  def main(args: Array[String]): Unit = {
    val machines = parseInput(utils.loadInputAsStrings("2025/day10.txt"))

    println(s"Part 1: ${machines.map(minButtonPressCountForLights).sum}")
    println(s"Part 2: ${machines.map(minButtonPressCountForJoltages).sum}")
  }

  final case class Machine(lights: Vector[Int], buttons: Seq[Vector[Int]], joltages: Vector[Int])

  def minButtonPressCountForLights(m: Machine): Long = {
    val (buttons, target) = (m.buttons.map(toIntBitMap), toIntBitMap(m.lights))
    @annotation.tailrec
    def dfs(toVisit: Seq[Int], seen: Set[Int], steps: Int): Long =
      if (toVisit.contains(target)) steps
      else {
        val nextToVisit = toVisit.flatMap(state => buttons.map(_ ^ state).filterNot(seen.contains))
        dfs(nextToVisit, seen ++ nextToVisit, steps + 1)
      }
    dfs(toVisit = Seq(0), seen = Set(0), steps = 0)
  }

  def minButtonPressCountForJoltages(machine: Machine): Long = Using.resource(new Context()) { c =>
    val solver = c.mkOptimize()
    val vars   = machine.buttons.indices.map(i => c.mkIntConst(s"b$i").tap(v => solver.Add(c.mkGe(v, c.mkInt(0)))))

    machine.joltages.zipWithIndex.foreach { case (joltage, i) =>
      val terms = machine.buttons.zipWithIndex.collect { case (button, j) if button(i) == 1 => vars(j) }
      solver.Add(c.mkEq(c.mkAdd(terms: _*), c.mkInt(joltage)))
    }

    solver.MkMinimize(c.mkAdd(vars: _*)).tap(_ => solver.Check())
    vars.map(v => solver.getModel.eval(v, false).toString.toLong).sum
  }

  private def parseInput(lines: Seq[String]) = lines.map { line =>
    val lights  = line.slice(1, line.indexOf('(') - 2).map(ch => if (ch == '#') 1 else 0).toVector
    val joltage = line.slice(line.indexOf('{') + 1, line.indexOf('}')).split(',').map(_.toInt).toVector
    val buttons = line.slice(line.indexOf('('), line.lastIndexOf(')') + 1).split(' ').toSeq.map { s =>
      val indices = s.slice(1, s.length - 1).split(',').map(_.toInt).toSet
      Vector.tabulate(lights.length)(i => if (indices.contains(i)) 1 else 0)
    }
    Machine(lights, buttons, joltage)
  }

  private def toIntBitMap(bits: Vector[Int]): Int = 
    bits.zipWithIndex.map { case (bit, idx) => bit << idx }.sum

}
