package com.github.cosminci.aoc._2018

import cats.syntax.semigroup._
import com.github.cosminci.aoc.utils

object Day6 {

  def main(args: Array[String]): Unit = {
    val coords = parseInput(utils.loadInputAsStrings("2018/day6.txt"))

    println(s"Part 1: ${largestAreaBoundedByCoords(coords)}")
    println(s"Part 2: ${regionWithinDistanceOfAllCoords(coords)}")
  }

  final case class Pos(x: Int, y: Int)
  final case class State(ownership: Map[Pos, Set[Pos]], owned: Set[Pos])

  def largestAreaBoundedByCoords(coords: Seq[Pos]): Int = {
    val (minX, maxX) = (coords.map(_.x).min, coords.map(_.x).max)
    val (minY, maxY) = (coords.map(_.y).min, coords.map(_.y).max)
    val iters        = (maxX - minX).max(maxY - minY) / 2

    val initialState = State(ownership = coords.map(p => p -> Set(p)).toMap, owned = coords.toSet)
    val Seq(s1, s2)  = Iterator.iterate(initialState)(expand).sliding(2).drop(iters).next()

    s1.ownership.collect { case (src, owned) if s2.ownership(src) == owned => owned.size }.max
  }

  def regionWithinDistanceOfAllCoords(coords: Seq[Pos]): Int = {
    val (minX, maxX) = (coords.map(_.x).min, coords.map(_.x).max)
    val (minY, maxY) = (coords.map(_.y).min, coords.map(_.y).max)

    val dists = for {
      x <- minX to maxX
      y <- minY to maxY
    } yield dist(Pos(x, y), coords)

    dists.count(_ < 10_000)
  }

  private def expand(state: State) = {
    val newChildren = state.ownership.toList
      .flatMap { case (src, children) => children.flatMap(neighbours).diff(state.owned).map(src -> _) }
      .groupMapReduce { case (_, child) => child } { case (src, _) => Seq(src) } { case (s1, s2) => s1 ++ s2 }.toList
      .collect { case (child, sources) if sources.size == 1 => sources.head -> child }
      .groupMapReduce { case (src, _) => src } { case (_, child) => Set(child) } { case (s1, s2) => s1 ++ s2 }

    State(state.ownership.combine(newChildren), state.owned ++ newChildren.values.flatten)
  }

  private def dist(p: Pos, coords: Seq[Pos]) =
    coords.map { case Pos(x, y) => (p.x - x).abs + (p.y - y).abs }.sum

  private def neighbours(p: Pos) =
    Seq((0, 1), (0, -1), (1, 0), (-1, 0)).map { case (dx, dy) => Pos(p.x + dx, p.y + dy) }

  private def parseInput(input: Seq[String]) =
    input.map { case s"$x, $y" => Pos(x.toInt, y.toInt) }

}
