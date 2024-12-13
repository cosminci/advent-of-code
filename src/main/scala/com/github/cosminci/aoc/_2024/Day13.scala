package com.github.cosminci.aoc._2024

import com.github.cosminci.aoc.utils
import monocle.syntax.all._

import scala.collection.immutable.TreeSet

object Day13 {

  def main(args: Array[String]): Unit = {
    val machines = parseInput(utils.loadInputAsStrings("2024/day13.txt"))

    println(s"Part 1: ${machines.map(fewestTokensPqueue).sum}")
    println(s"Part 2: ${machines.map(movePrize).map(fewestTokensAlgebra).sum}")
  }

  final case class Button(dx: Int, dy: Int)
  final case class Pos(x: Long, y: Long)
  final case class Machine(btnA: Button, btnB: Button, prize: Pos)
  final case class State(tokensUsed: Long, stepsA: Int, stepsB: Int, pos: Pos)

  implicit val stateOrdering: Ordering[State] = Ordering.by(s => (s.tokensUsed, s.stepsA, s.stepsB))

  def fewestTokensPqueue(machine: Machine): Long = {
    @annotation.tailrec
    def dfs(toVisit: TreeSet[State]): Long =
      toVisit.headOption match {
        case None => 0
        case Some(State(used, stepsA, stepsB, pos)) =>
          if (pos == machine.prize) used
          else {
            val stateA = State(used + 3, stepsA + 1, stepsB, Pos(pos.x + machine.btnA.dx, pos.y + machine.btnA.dy))
            val stateB = State(used + 1, stepsA, stepsB + 1, Pos(pos.x + machine.btnB.dx, pos.y + machine.btnB.dy))
            dfs(toVisit.tail ++ Option.when(stateA.stepsA <= 100)(stateA) ++ Option.when(stateB.stepsB <= 100)(stateB))
          }
      }
    dfs(toVisit = TreeSet(State(tokensUsed = 0, stepsA = 0, stepsB = 0, pos = Pos(x = 0, y = 0))))
  }

  def fewestTokensAlgebra(machine: Machine): Long = {
    val Machine(Button(dxA, dyA), Button(dxB, dyB), Pos(x, y)) = machine

    val B = (y * dxA - x * dyA) / (dyB * dxA - dxB * dyA)
    val A = (x - B * dxB) / dxA

    if (A >= 0 && B >= 0 && B * dxB + A * dxA == x && B * dyB + A * dyA == y) A * 3 + B else 0
  }

  private def movePrize(machine: Machine) =
    machine.focus(_.prize).modify(p => Pos(p.x + 10_000_000_000_000L, p.y + 10_000_000_000_000L))

  private def parseInput(lines: Seq[String]) =
    lines.grouped(4).map(_.toList)
      .map { case buttonA :: buttonB :: prize :: _ =>
        Machine(parseButton(buttonA), parseButton(buttonB), parsePrize(prize))
      }.toSeq

  private def parseButton(line: String) =
    line match { case s"Button $_: X+$dx, Y+$dy" => Button(dx.toInt, dy.toInt) }

  private def parsePrize(line: String) =
    line match { case s"Prize: X=$x, Y=$y" => Pos(x.toInt, y.toInt) }

}
