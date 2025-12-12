package com.github.cosminci.aoc._2025

import com.github.cosminci.aoc.utils.loadInputAsStrings

import scala.util.chaining.scalaUtilChainingOps

object Day12 {

  def main(args: Array[String]): Unit = {
    val (shapes, regions) = parseInput(loadInputAsStrings("2025/day12.txt"))

    println(s"Part 1: ${countFulfillableRegions(shapes, regions)}")
  }

  type Shape = Set[(Int, Int)]
  final case class Region(width: Int, height: Int, counts: Seq[Int])

  def countFulfillableRegions(shapes: Seq[Shape], regions: Seq[Region]): Int = {
    val orientations  = shapes.map(generateOrientations)
    val maxShapeAreas = shapes.map(s => maxShapePos(s).pipe { case (maxR, maxC) => (maxR + 1) * (maxC + 1) })

    regions.count { region =>
      val regionArea      = region.width * region.height
      val minAreaRequired = region.counts.zipWithIndex.map { case (count, i) => count * shapes(i).size }.sum
      val maxAreaRequired = region.counts.zipWithIndex.map { case (count, i) => count * maxShapeAreas(i) }.sum

      if (maxAreaRequired <= regionArea) true
      else if (minAreaRequired > regionArea) false
      else canFitPieces(region, orientations)
    }
  }

  private def canFitPieces(region: Region, orientations: Seq[Seq[Shape]]): Boolean = {
    val (m, n) = (region.height, region.width)

    def dfs(occupied: Set[(Int, Int)] = Set.empty, shapesToPlace: List[Int]): Boolean =
      shapesToPlace match {
        case Nil => true
        case shapeToPlace :: remainingShapesToPlace =>
          (0 until m).iterator.exists { r =>
            (0 until n).iterator.exists { c =>
              orientations(shapeToPlace).iterator.exists { orientation =>
                val toOccupy = orientation.map { case (dr, dc) => (r + dr, c + dc) }
                if (toOccupy.exists { case (pr, pc) => pr >= m || pc >= n || occupied.contains((pr, pc)) }) false
                else dfs(occupied ++ toOccupy, remainingShapesToPlace)
              }
            }
          }
      }

    dfs(shapesToPlace = region.counts.zipWithIndex.flatMap { case (count, i) => List.fill(count)(i) }.toList)
  }

  private def generateOrientations(shape: Shape) = {
    val rotations = (0 to 3).scanLeft(shape) { (s, _) => s.map { case (r, c) => (c, -r) } }
    val flips     = rotations.map(_.map { case (r, c) => (-r, c) })
    (rotations ++ flips).map(shiftToOrigin).distinct
  }

  private def shiftToOrigin(shape: Shape) =
    minShapePos(shape).pipe { case (minR, minC) => shape.map { case (r, c) => (r - minR, c - minC) } }

  private def minShapePos(shape: Shape) =
    shape.foldLeft((Int.MaxValue, Int.MaxValue)) { case ((minR, minC), (r, c)) => (minR.min(r), minC.min(c)) }

  private def maxShapePos(shape: Shape) =
    shape.foldLeft((Int.MinValue, Int.MinValue)) { case ((maxR, maxC), (r, c)) => (maxR.max(r), maxC.max(c)) }

  private def parseInput(input: Seq[String]) = {
    val sections = input.mkString("\n").split("\n\n")
    val shapes   = sections.init.map(parseShape)
    val regions = sections.last.split("\n").map { case s"${w}x$h: $vals" =>
      Region(w.toInt, h.toInt, vals.split(" ").map(_.toInt).toSeq)
    }
    (shapes.toSeq, regions.toSeq)
  }

  private def parseShape(shapeBlock: String) =
    shapeBlock.split("\n").tail.zipWithIndex.flatMap { case (line, r) =>
      line.zipWithIndex.flatMap { case (ch, c) => Option.when(ch == '#')((r, c)) }
    }.toSet

}
