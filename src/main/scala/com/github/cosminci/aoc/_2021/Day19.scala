package com.github.cosminci.aoc._2021

import com.github.cosminci.aoc.utils

import scala.annotation.tailrec

// Kudos to https://github.com/sim642
object Day19 {
  def main(args: Array[String]): Unit = {
    val scanners = parseScanners(utils.loadInputAsStrings("2021/day19.txt"))

    val (beacons, alignments) = alignScanners(scanners)
    println(s"Part I: ${beacons.size}")
    println(s"Part II: ${alignments.combinations(2).map(p => distance(p.head, p.last)).max}")
  }

  case class Position(x: Int, y: Int, z: Int) {
    def +(other: Position): Position = Position(this.x + other.x, this.y + other.y, this.z + other.z)
    def -(other: Position): Position = Position(this.x - other.x, this.y - other.y, this.z - other.z)
  }

  private def parseScanners(input: Seq[String]): Seq[Set[Position]] = {
    def parseScanner(input: Seq[String]): (Set[Position], Seq[String]) = {
      val positions = input
        .drop(1)
        .takeWhile(_.nonEmpty)
        .map(s => s.split(',').map(_.toInt))
        .map { case Array(x, y, z) => Position(x, y, z) }
        .toSet
      (positions, input.drop(positions.size + 2))
    }
    val (scanner, rest) = parseScanner(input)
    scanner +: Option.when(rest.nonEmpty)(parseScanners(rest)).getOrElse(Seq.empty)
  }

  private def alignScanners(scanners: Seq[Set[Position]]): (Set[Position], Seq[Position]) = {
    @tailrec
    def dfs(
        scanners: Seq[(Set[Position], Int)],
        beacons: Set[Position],
        alignments: Seq[Position]
    ): (Set[Position], Seq[Position]) =
      if (scanners.isEmpty) (beacons, alignments)
      else {
        val matchResults = for {
          (scanner, idx)      <- scanners.view
          (aligned, distance) <- matchScannerToBeacons(beacons, scanner)
        } yield (idx, aligned, distance)
        val (idx, aligned, d) = matchResults.head
        val newBeacons        = beacons ++ aligned
        val newScanners       = scanners.filterNot(_._2 == idx)
        val newAlignments     = alignments :+ d
        dfs(newScanners, newBeacons, newAlignments)
      }

    dfs(scanners.tail.zipWithIndex, scanners.head, Seq(Position(0, 0, 0)))
  }

  private def matchScannerToBeacons(
      beacons: Set[Position],
      scanner: Set[Position]
  ): Option[(Set[Position], Position)] = {
    val matches = for {
      orientation       <- orientations(scanner).view
      (distance, count) <- groupDistances(beacons, orientation).view if count >= 12
    } yield (orientation.map(_ + distance), distance)
    matches.headOption
  }

  private def groupDistances(beacons: Set[Position], orientation: Set[Position]) = {
    val distances = for {
      p1 <- beacons.view
      p2 <- orientation.view
    } yield p1 - p2
    distances.groupMapReduce(identity)(_ => 1)(_ + _)
  }

  private def orientations(scanner: Set[Position]): Seq[Set[Position]] =
    scanner.toSeq.map(orientations).transpose.map(_.toSet)

  private def orientations(position: Position): Seq[Position] = {
    val Position(x, y, z) = position
    Seq(
      Position(x, y, z),
      Position(-y, x, z),
      Position(-x, -y, z),
      Position(y, -x, z),
      Position(-x, y, -z),
      Position(y, x, -z),
      Position(x, -y, -z),
      Position(-y, -x, -z),
      Position(-z, y, x),
      Position(-z, x, -y),
      Position(-z, -y, -x),
      Position(-z, -x, y),
      Position(z, y, -x),
      Position(z, x, y),
      Position(z, -y, x),
      Position(z, -x, -y),
      Position(x, -z, y),
      Position(-y, -z, x),
      Position(-x, -z, -y),
      Position(y, -z, -x),
      Position(x, z, -y),
      Position(-y, z, -x),
      Position(-x, z, y),
      Position(y, z, x)
    )
  }

  private def distance(p1: Position, p2: Position) =
    math.abs(p1.x - p2.x) + math.abs(p1.y - p2.y) + math.abs(p1.z - p2.z)
}
