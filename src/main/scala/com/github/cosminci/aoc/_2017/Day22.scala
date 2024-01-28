package com.github.cosminci.aoc._2017

import com.github.cosminci.aoc.utils

import scala.util.chaining._

object Day22 {

  def main(args: Array[String]): Unit = {
    val (start, infected) = parseInput(utils.loadInputAsStrings("2017/day22.txt"))

    println(s"Part 1: ${infectionsCaused(start, infected, states = 2, iters = 10_000)}")
    println(s"Part 2: ${infectionsCaused(start, infected, states = 4, iters = 10_000_000)}")
  }

  final case class Pos(r: Int, c: Int)

  private val dirs = Seq((-1, 0), (0, 1), (1, 0), (0, -1))

  def infectionsCaused(start: Pos, infected: Set[Pos], states: Int, iters: Int): Int = {
    val infectedState = if (states == 2) 1 else 2
    val dirFn         = if (states == 2) updateDirTwoStates _ else updateDirFourStates _

    @annotation.tailrec
    def dfs(i: Int, pos: Pos, dir: Int, nodes: Map[Pos, Int], cnt: Int): Int =
      if (i == iters) cnt
      else {
        val currState = nodes.getOrElse(pos, 0)
        val newDir    = dirFn(dir, currState)
        val newState  = (currState + 1) % states
        val newPos    = dirs(newDir).pipe { case (dr, dc) => Pos(pos.r + dr, pos.c + dc) }
        val newCnt    = cnt + (if (newState == infectedState) 1 else 0)
        dfs(i + 1, newPos, newDir, nodes + (pos -> newState), newCnt)
      }

    dfs(i = 0, start, dir = 0, infected.map(_ -> infectedState).toMap, cnt = 0)
  }

  private def updateDirTwoStates(dir: Int, state: Int) =
    if (state == 1) turnRight(dir) else turnLeft(dir)

  private def updateDirFourStates(dir: Int, state: Int) =
    if (state == 0) turnLeft(dir)
    else if (state == 1) dir
    else if (state == 2) turnRight(dir)
    else (dir + 2) % dirs.length

  private def turnRight(dir: Int) = (dir + 1)               % dirs.length
  private def turnLeft(dir: Int)  = (dir - 1 + dirs.length) % dirs.length

  private def parseInput(input: Seq[String]) = {
    val start    = Pos(input.length / 2, input.head.length / 2)
    val indices  = input.indices.flatMap(r => input(r).indices.map(c => Pos(r, c)))
    val infected = indices.filter(p => input(p.r)(p.c) == '#').toSet
    (start, infected)
  }

}
