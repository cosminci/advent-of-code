package com.github.cosminci.aoc._2024

import cats.syntax.either._
import com.github.cosminci.aoc.utils

object Day6 {

  def main(args: Array[String]): Unit = {
    val grid = utils.loadInputAsStrings("2024/day6.txt")

    println(s"Part 1: ${distinctPositionsVisited(grid)}")
    println(s"Part 2: ${viableObstructionLocations(grid)}")
  }

  final case class Pos(r: Int, c: Int)
  final case class State(pos: Pos, dir: Char)

  def distinctPositionsVisited(grid: Seq[String]): Int =
    walkPath(findGuard(grid), grid).fold(_ => 0, _.visited.map(_.pos).size)

  def viableObstructionLocations(grid: Seq[String]): Int = {
    val start = findGuard(grid)
    (walkPath(start, grid).fold(_ => Set.empty[Pos], _.visited.map(_.pos)) - start.pos)
      .count(pos => walkPath(start, grid.updated(pos.r, grid(pos.r).updated(pos.c, '#'))).fold(_ => true, _ => false))
  }

  final case object FoundCycle
  final case class ExitedGrid(visited: Set[State])

  private def walkPath(state: State, grid: Seq[String]): Either[FoundCycle.type, ExitedGrid] = {
    @annotation.tailrec
    def dfs(state: State, visited: Set[State]): Either[FoundCycle.type, ExitedGrid] = {
      val nextPos = move(state)
      if (outsideBounds(nextPos, grid)) ExitedGrid(visited).asRight
      else if (grid(nextPos.r)(nextPos.c) == '#') dfs(State(state.pos, rotateDir(state.dir)), visited)
      else if (visited.contains(State(nextPos, state.dir))) FoundCycle.asLeft
      else dfs(State(nextPos, state.dir), visited + State(nextPos, state.dir))
    }
    dfs(state, visited = Set(state))
  }

  private val moves              = Map('^' -> Pos(-1, 0), '>' -> Pos(0, 1), 'V' -> Pos(1, 0), '<' -> Pos(0, -1))
  private def move(state: State) = Pos(state.pos.r + moves(state.dir).r, state.pos.c + moves(state.dir).c)

  private val dirs                 = Seq('^', '>', 'V', '<')
  private def rotateDir(dir: Char) = dirs((dirs.indexOf(dir) + 1) % dirs.length)

  private def outsideBounds(p: Pos, grid: Seq[String]) = p.r.min(p.c) < 0 || p.r.max(p.c) == grid.length

  private def findGuard(grid: Seq[String]) =
    grid.indices
      .flatMap(r => grid(r).indices.map(c => Pos(r, c)))
      .collectFirst { case p @ Pos(r, c) if dirs.contains(grid(r)(c)) => State(p, grid(r)(c)) }
      .get

}
