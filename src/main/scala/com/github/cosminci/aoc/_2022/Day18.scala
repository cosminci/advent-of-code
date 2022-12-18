package com.github.cosminci.aoc._2022

import com.github.cosminci.aoc.utils

import scala.util.chaining._

object Day18 {

  def main(args: Array[String]): Unit = {
    val input  = utils.loadInputAsStrings("2022/day18.txt")
    val points = input.map(_.split(',').map(_.toInt).pipe { case Array(x, y, z) => Pos(x, y, z) }).toSet

    println(s"Part 1: ${totalSurfaceArea(points)}")
    println(s"Part 2: ${externalSurfaceArea(points)}")
  }

  def totalSurfaceArea(points: Set[Pos]): Int =
    points.toSeq.map(p => 6 - p.nei.intersect(points).size).sum

  def externalSurfaceArea(points: Set[Pos]): Int = {
    val boxMinBound = points.reduce(_ min _).pipe { case Pos(x, y, z) => Pos(x - 1, y - 1, z - 1) }
    val boxMaxBound = points.reduce(_ max _).pipe { case Pos(x, y, z) => Pos(x + 1, y + 1, z + 1) }

    Iterator
      .iterate((Seq(boxMinBound), Set.empty[Pos])) { case (curr +: toVisit, visited) =>
        curr.nei
          .filterNot(next => visited.contains(next) || points.contains(next))
          .filter(next => next.min(boxMinBound) == boxMinBound && next.max(boxMaxBound) == boxMaxBound)
          .pipe(validNei => (toVisit ++ validNei, visited ++ validNei + curr))
      }
      .dropWhile { case (toVisit, _) => toVisit.nonEmpty }
      .next()
      .pipe { case (_, externalReachable) => points.toSeq.map(_.nei.count(externalReachable.contains)).sum }
  }

  case class Pos(x: Int, y: Int, z: Int) {
    def min(other: Pos): Pos = Pos(x min other.x, y min other.y, z min other.z)
    def max(other: Pos): Pos = Pos(x max other.x, y max other.y, z max other.z)
    def nei: Set[Pos] =
      Set(Pos(x + 1, y, z), Pos(x - 1, y, z), Pos(x, y + 1, z), Pos(x, y - 1, z), Pos(x, y, z + 1), Pos(x, y, z - 1))
  }

}
