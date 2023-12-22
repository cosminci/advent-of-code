package com.github.cosminci.aoc._2023

import com.github.cosminci.aoc.utils
import monocle.syntax.all._

object Day22 {

  def main(args: Array[String]): Unit = {
    val input  = utils.loadInputAsStrings("2023/day22.txt")
    val bricks = layBricks(parseInput(input).sortBy(_.z.head))

    println(s"Part 1: ${countOptionalBricks(bricks)}")
    println(s"Part 2: ${sumBricksToFall(bricks)}")
  }

  final case class Brick(x: Range, y: Range, z: Range)

  def countOptionalBricks(bricks: Seq[Brick]): Int = {
    val bricksByEndZ = bricks.groupBy(_.z.last).view.mapValues(_.toSet).toMap
    val supporting   = computeSupporting(bricks)

    supporting.count { case (brick, supported) =>
      supported.forall { supportedBrick =>
        (bricksByEndZ(brick.z.last) - brick).exists(blocks(_, supportedBrick))
      }
    }
  }

  def sumBricksToFall(bricks: Seq[Brick]): Int = {
    val supporting  = computeSupporting(bricks)
    val supportedBy = reverseSupporting(supporting)

    @annotation.tailrec
    def dfs(toMove: Set[Brick], moved: Set[Brick]): Int = {
      val newMoved  = moved ++ toMove
      val newToMove = toMove.flatMap(supporting).filter(supportedBy(_).diff(newMoved).isEmpty)
      if (newToMove.isEmpty) newMoved.size - 1 else dfs(newToMove, newMoved)
    }

    bricks.map(b => dfs(toMove = Set(b), moved = Set.empty)).sum
  }

  private def computeSupporting(bricks: Seq[Brick]) = {
    val bricksByStartZ = bricks.groupBy(_.z.head).view.mapValues(_.toSet).toMap

    def bricksSupportedBy(brick: Brick): Set[Brick] =
      bricksByStartZ.getOrElse(brick.z.last + 1, Set.empty).filter(blocks(brick, _))

    bricks.map(b => b -> bricksSupportedBy(b)).toMap
  }

  private def layBricks(bricks: Seq[Brick]) =
    bricks.foldLeft(Seq.empty[Brick]) { (settledBricks, brick) =>
      val candidates = settledBricks.filter(blocks(_, brick))
      val maxZ       = candidates.map(_.z.max).maxOption.getOrElse(0)
      settledBricks :+ layBrick(brick, maxZ)
    }

  private def layBrick(brick: Brick, baseZ: Int): Brick =
    brick.focus(_.z).modify(z => baseZ + 1 to (baseZ + z.length))

  private def blocks(settled: Brick, falling: Brick): Boolean =
    overlap(settled.x, falling.x) && overlap(settled.y, falling.y)

  private def overlap(r1: Range, r2: Range): Boolean =
    (r1.head <= r2.last && r1.last >= r2.head) ||
      (r2.head <= r1.last && r2.last >= r1.head)

  private def reverseSupporting(supporting: Map[Brick, Set[Brick]]) =
    supporting.toSeq
      .flatMap { case (brick, supported) => supported.map(_ -> brick) }
      .groupMap { case (supported, _) => supported } { case (_, supporting) => supporting }
      .view.mapValues(_.toSet).toMap

  private def parseInput(input: Seq[String]) =
    input.map { case s"$x1,$y1,$z1~$x2,$y2,$z2" =>
      Brick(x1.toInt to x2.toInt, y1.toInt to y2.toInt, z1.toInt to z2.toInt)
    }

}
