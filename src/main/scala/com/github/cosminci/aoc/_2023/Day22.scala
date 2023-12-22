package com.github.cosminci.aoc._2023

import com.github.cosminci.aoc.utils
import monocle.syntax.all._

object Day22 {

  def main(args: Array[String]): Unit = {
    val input  = utils.loadInputAsStrings("2023/day22.txt")
    val bricks = layBricks(parseInput(input).sortBy(_.z.head))

    println(s"Part 1: ${countDisintegrableBricks(bricks)}")
  }

  final case class Brick(x: Range, y: Range, z: Range)

  def countDisintegrableBricks(bricks: Seq[Brick]): Int = {
    val bricksByEndZ   = bricks.groupBy(_.z.last).view.mapValues(_.toSet).toMap
    val bricksByStartZ = bricks.groupBy(_.z.head).view.mapValues(_.toSet).toMap

    def bricksSupportedOnlyBy(brick: Brick): Set[Brick] =
      bricksByStartZ.get(brick.z.last + 1).map(_.filter(blocks(brick, _))) match {
        case None => Set.empty
        case Some(supportedBricks) =>
          supportedBricks.filterNot { supportedBrick =>
            val otherSupporting = bricksByEndZ(brick.z.last) - brick
            otherSupporting.exists(blocks(_, supportedBrick))
          }
      }

    bricks.count(bricksSupportedOnlyBy(_).isEmpty)
  }

  private def layBricks(bricks: Seq[Brick]) =
    bricks.foldLeft(Seq.empty[Brick]) { (stableBricks, brick) =>
      val candidates = stableBricks.filter(blocks(_, brick))
      val maxZ       = candidates.map(_.z.max).maxOption.getOrElse(0)
      stableBricks :+ layBrick(brick, maxZ)
    }

  private def layBrick(brick: Brick, baseZ: Int): Brick =
    brick.focus(_.z).modify(z => baseZ + 1 to (baseZ + z.length))

  private def blocks(stable: Brick, falling: Brick): Boolean =
    overlap(stable.x, falling.x) && overlap(stable.y, falling.y)

  private def overlap(r1: Range, r2: Range): Boolean =
    (r1.head <= r2.last && r1.last >= r2.head) ||
      (r2.head <= r1.last && r2.last >= r1.head)

  private def parseInput(input: Seq[String]) = input.map { case s"$x1,$y1,$z1~$x2,$y2,$z2" =>
    Brick(x1.toInt to x2.toInt, y1.toInt to y2.toInt, z1.toInt to z2.toInt)
  }

}
