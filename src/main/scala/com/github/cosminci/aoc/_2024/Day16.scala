package com.github.cosminci.aoc._2024

import com.github.cosminci.aoc.utils

import scala.collection.immutable.TreeSet

object Day16 {

  def main(args: Array[String]): Unit = {
    val grid = utils.loadInputAsStrings("2024/day16.txt")

    println(s"Part 1: ${lowestScore(grid)}")
    println(s"Part 2: ${bestTileCount(grid)}")
  }

  final case class Pos(r: Int, c: Int)
  final case class State(pos: Pos, dir: (Int, Int))
  final case class Candidate(state: State, score: Int, path: Seq[Pos])

  private val directions = Seq((0, 1), (-1, 0), (0, -1), (1, 0))

  implicit val positionOrdering: Ordering[Pos] = Ordering.by { case Pos(r, c) => (r, c) }
  implicit val candidateOrdering: Ordering[Candidate] =
    Ordering.by { case Candidate(state, score, path) => (score, state.pos, state.dir, path.hashCode()) }

  def lowestScore(grid: Seq[String]): Int =
    bestPaths(grid).head.score

  def bestTileCount(grid: Seq[String]): Int =
    bestPaths(grid).flatMap(_.path).distinct.size

  private def bestPaths(grid: Seq[String]) = {
    def dfs(toVisit: TreeSet[Candidate], visited: Map[State, Int], bestScore: Int): Seq[Candidate] = {
      val curr = toVisit.head
      if (curr.score > bestScore) Seq.empty
      else if (grid(curr.state.pos.r)(curr.state.pos.c) == 'E') curr +: dfs(toVisit.tail, visited, curr.score)
      else {
        val (nextToVisit, nextVisited) = findNextCandidates(curr, grid).foldLeft((toVisit.tail, visited)) {
          case ((toVisit, visited), next @ Candidate(nextState, nextScore, _)) =>
            if (visited.get(nextState).exists(_ < nextScore)) (toVisit, visited)
            else (toVisit + next, visited.updated(nextState, nextScore))
        }
        dfs(nextToVisit, nextVisited, bestScore = Int.MaxValue)
      }
    }
    val startCandidate = findStartCandidate(grid)
    dfs(toVisit = TreeSet(startCandidate), visited = Map(startCandidate.state -> startCandidate.score), bestScore = 0)
  }

  private def isBlocked(pos: Pos, grid: Seq[String]) = grid(pos.r)(pos.c) == '#'

  private def findNextCandidates(curr: Candidate, grid: Seq[String]) = {
    val Candidate(State(pos, dir @ (dr, dc)), score, path) = curr

    val dirIdx    = directions.indexOf(dir)
    val turnLeft  = Candidate(State(pos, directions((dirIdx + 1) % directions.length)), score + 1000, path)
    val turnRight = Candidate(State(pos, directions((dirIdx + 3) % directions.length)), score + 1000, path)
    val aheadPos  = Pos(pos.r + dr, pos.c + dc)
    val moveAhead = Candidate(State(aheadPos, dir), score + 1, path :+ aheadPos)

    Seq(turnLeft, turnRight) ++ Option.when(!isBlocked(aheadPos, grid))(moveAhead)
  }

  private def findStartCandidate(grid: Seq[String]) = {
    val start = grid.indices.collectFirst { case r if grid(r).contains('S') => Pos(r, grid(r).indexOf("S")) }.get
    Candidate(State(start, dir = (0, 1)), score = 0, path = Seq(start))
  }

}
