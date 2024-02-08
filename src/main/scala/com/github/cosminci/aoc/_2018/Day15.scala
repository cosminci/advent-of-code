package com.github.cosminci.aoc._2018

import com.github.cosminci.aoc.utils

import scala.collection.immutable.TreeSet
import scala.util.chaining._

object Day15 {

  def main(args: Array[String]): Unit = {
    val (grid, state) = parseInput(utils.loadInputAsStrings("2018/day15.txt"))

    println(s"Part 1: ${outcomeOfCombat(grid, state).pipe { case (score, _) => score }}")
    println(s"Part 2: ${outcomeOfCombatFlawlessElfVictory(grid, state)}")
  }

  final case class Pos(r: Int, c: Int)
  final case class Actor(pos: Pos, hp: Int, isElf: Boolean)
  implicit val posOrdering: Ordering[Pos]     = Ordering.by(p => (p.r, p.c))
  implicit val actorOrdering: Ordering[Actor] = Ordering.by(_.pos)

  type Path = Seq[Pos]

  sealed trait RoundResult
  case class NoOpponentsLeftPartialRound(survivors: TreeSet[Actor]) extends RoundResult
  case class NoOpponentsLeftFullRound(survivors: TreeSet[Actor])    extends RoundResult
  case class Continue(actors: TreeSet[Actor])                       extends RoundResult

  def outcomeOfCombat(grid: Seq[String], actors: TreeSet[Actor], elfPower: Int = 3): (Int, TreeSet[Actor]) = {
    @annotation.tailrec
    def dfs(round: Int, actors: TreeSet[Actor]): (Int, TreeSet[Actor]) =
      playRound(grid, actors, elfPower) match {
        case NoOpponentsLeftPartialRound(survivors) => ((round - 1) * survivors.toSeq.map(_.hp).sum, survivors)
        case NoOpponentsLeftFullRound(survivors)    => (round * survivors.toSeq.map(_.hp).sum, survivors)
        case Continue(newActors)                    => dfs(round + 1, newActors)
      }

    dfs(round = 1, actors)
  }

  def outcomeOfCombatFlawlessElfVictory(grid: Seq[String], actors: TreeSet[Actor]): Int = {
    val elfCount = actors.count(_.isElf)

    @annotation.tailrec
    def search(l: Int, r: Int): Int =
      if (l >= r) outcomeOfCombat(grid, actors, l).pipe { case (score, _) => score }
      else {
        val mid            = (l + r) / 2
        val (_, survivors) = outcomeOfCombat(grid, actors, mid)
        if (survivors.count(_.isElf) == elfCount) search(l, mid)
        else search(mid + 1, r)
      }

    search(l = 4, r = 200)
  }

  private def playRound(grid: Seq[String], actors: TreeSet[Actor], elfPower: Int) = {
    def attack(done: TreeSet[Actor], remaining: TreeSet[Actor], attacking: Actor, adversary: Actor) = {
      val attacked = adversary.copy(hp = adversary.hp - (if (attacking.isElf) elfPower else 3))
      if (attacked.hp <= 0) dfs(remaining.excl(adversary), done.excl(adversary).incl(attacking))
      else if (remaining.contains(adversary)) dfs(remaining.excl(adversary).incl(attacked), done.incl(attacking))
      else dfs(remaining, done.excl(adversary).incl(attacked).incl(attacking))
    }

    @annotation.tailrec
    def dfs(initial: TreeSet[Actor], done: TreeSet[Actor]): RoundResult =
      if (initial.isEmpty)
        if (done.map(_.isElf).size == 1) NoOpponentsLeftFullRound(done)
        else Continue(done)
      else {
        val (remaining, moved, opponents) = move(grid, initial, done)
        if (opponents.isEmpty) NoOpponentsLeftPartialRound(remaining ++ done + moved)
        else
          findAdjacentAdversary(moved.pos, opponents) match {
            case None            => dfs(remaining, done.incl(moved))
            case Some(adversary) => attack(done, remaining, moved, adversary)
          }
      }

    dfs(initial = actors, done = TreeSet.empty)
  }

  private def move(grid: Seq[String], initial: TreeSet[Actor], done: TreeSet[Actor]) = {
    val actor        = initial.head
    val remaining    = initial.excl(actor)
    val others       = remaining ++ done
    val opponents    = others.filter(_.isElf != actor.isElf)
    val targetSquare = findTargetSquare(actor, others, opponents, grid)
    val moved        = actor.copy(pos = targetSquare)
    (remaining, moved, opponents)
  }

  private def findTargetSquare(actor: Actor, others: TreeSet[Actor], opponents: TreeSet[Actor], grid: Seq[String]) = {
    @annotation.tailrec
    def dfs(paths: Vector[Path], visited: Set[Pos]): Pos =
      if (paths.isEmpty) actor.pos
      else
        paths.filter(path => findAdjacentAdversary(path.last, opponents).isDefined) match {
          case paths if paths.nonEmpty =>
            val chosenPath = paths.minBy(_.last)
            if (chosenPath.head != chosenPath.last) chosenPath(1) else actor.pos
          case Seq() =>
            val (nei, newVisited) = paths.foldLeft(Vector.empty[Path], visited) { case ((newPaths, visited), path) =>
              val nei = neighbours(path.last, grid, others, visited)
              (newPaths ++ nei.map(path :+ _), visited ++ nei)
            }
            dfs(nei, newVisited)
        }

    dfs(paths = Vector(Vector(actor.pos)), visited = Set(actor.pos))
  }

  private def neighbours(pos: Pos, grid: Seq[String], actors: TreeSet[Actor], visited: Set[Pos]) =
    Seq((-1, 0), (0, -1), (0, 1), (1, 0))
      .map { case (dr, dc) => Pos(pos.r + dr, pos.c + dc) }
      .filter(p => grid(p.r)(p.c) != '#' && actors.forall(_.pos != p) && !visited.contains(p))

  private def findAdjacentAdversary(pos: Pos, targets: TreeSet[Actor]) =
    targets.filter(t => (t.pos.r - pos.r).abs + (t.pos.c - pos.c).abs == 1).minByOption(_.hp)

  private def parseInput(grid: Seq[String]) = {
    val actors    = TreeSet.from(findActors(grid))
    val cleanGrid = grid.map(_.replace('E', '.').replace('G', '.'))
    (cleanGrid, actors)
  }

  private def findActors(grid: Seq[String]) =
    grid.indices.flatMap { r =>
      grid(r).indices.collect {
        case c if "EG".contains(grid(r)(c)) =>
          Actor(Pos(r, c), 200, isElf = grid(r)(c) == 'E')
      }
    }

}
