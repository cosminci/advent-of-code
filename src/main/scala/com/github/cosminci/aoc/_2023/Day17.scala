package com.github.cosminci.aoc._2023

import com.github.cosminci.aoc.utils
import monocle.syntax.all._

import scala.collection.immutable.TreeSet

object Day17 {

  def main(args: Array[String]): Unit = {
    val grid = utils.loadInputAsStrings("2023/day17.txt").map(_.map(_ - '0'))

    println(s"Part 1: ${minimumHeatLoss(grid, minStreak = 0, maxStreak = 3)}")
    println(s"Part 2: ${minimumHeatLoss(grid, minStreak = 4, maxStreak = 10)}")
  }

  private val directions = Seq(Pos(0, 1), Pos(0, -1), Pos(-1, 0), Pos(1, 0))

  final case class Streak(dir: Pos, len: Int)
  final case class Pos(r: Int, c: Int)
  final case class State(pos: Pos, streak: Streak)

  implicit val stateOrd: Ordering[State] =
    Ordering.by { case State(pos, streak) => (pos.r, pos.c, streak.dir.r, streak.dir.c, -streak.len) }

  def minimumHeatLoss(grid: Seq[Seq[Int]], minStreak: Int, maxStreak: Int): Int = {
    val starts = Seq(
      (State(Pos(1, 0), Streak(dir = Pos(1, 0), len = 1)), grid(1)(0)),
      (State(Pos(0, 1), Streak(dir = Pos(0, 1), len = 1)), grid(0)(1))
    )
    val end = Pos(grid.length - 1, grid.head.length - 1)

    @annotation.tailrec
    def dfs(toVisit: TreeSet[(State, Int)], visited: Map[State, Int]): Int = {
      val (state, heatLoss) = toVisit.head
      if (state.pos == end && state.streak.len >= minStreak) heatLoss
      else {
        val (nextToVisit, nextVisited) = nextStates(state, minStreak, maxStreak, grid).foldLeft(toVisit.tail, visited) {
          case ((toVisit, visited), nextState) =>
            val nextHeatLoss = heatLoss + grid(nextState.pos.r)(nextState.pos.c)
            if (visited.get(nextState).exists(_ <= nextHeatLoss)) (toVisit, visited)
            else (toVisit + ((nextState, nextHeatLoss)), visited.updated(nextState, nextHeatLoss))
        }
        dfs(nextToVisit, nextVisited)
      }
    }

    val toVisit = TreeSet.from(starts)(Ordering.by { case (s, heatLoss) => (heatLoss, s) })
    dfs(toVisit, visited = Map.from(starts))
  }

  private def nextStates(state: State, minStreak: Int, maxStreak: Int, grid: Seq[Seq[Int]]) =
    possibleDirs(state.streak, minStreak, maxStreak)
      .map(buildNextState(state, _))
      .filter(s => s.streak.len <= maxStreak && isValid(s.pos, grid))

  private def buildNextState(state: State, dir: Pos) = {
    val nextPos    = Pos(state.pos.r + dir.r, state.pos.c + dir.c)
    val nextStreak = if (dir == state.streak.dir) state.streak.focus(_.len).modify(_ + 1) else Streak(dir, len = 1)
    State(nextPos, nextStreak)
  }

  private def possibleDirs(streak: Streak, minStreak: Int, maxStreak: Int) =
    if (streak.len < minStreak) Seq(streak.dir)
    else if (streak.len < maxStreak) directions.filterNot(isReverse(_, streak.dir))
    else directions.filterNot(d => isReverse(d, streak.dir) || d == streak.dir)

  private def isReverse(currDir: Pos, prevDir: Pos) =
    prevDir.r == -currDir.r && prevDir.c == -currDir.c

  private def isValid(pos: Pos, grid: Seq[Seq[Int]]) =
    pos.r >= 0 && pos.r < grid.length && pos.c >= 0 && pos.c < grid(pos.r).length

}
