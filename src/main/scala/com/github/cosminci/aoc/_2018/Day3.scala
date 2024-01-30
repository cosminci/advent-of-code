package com.github.cosminci.aoc._2018

import com.github.cosminci.aoc.utils

object Day3 {

  def main(args: Array[String]): Unit = {
    val claims = parseInput(utils.loadInputAsStrings("2018/day3.txt"))

    println(s"Part 1: ${contestedSquareInches(claims)}")
    println(s"Part 2: ${nonOverlappingClaim(claims)}")
  }

  final case class Claim(id: Int, x: Int, y: Int, w: Int, h: Int)

  def contestedSquareInches(claims: Seq[Claim]): Int =
    claims.foldLeft(Map.empty[(Int, Int), Int]) { case (posToClaimCount, claim) =>
      val newClaimCounts = for {
        x <- claim.x until claim.x + claim.w
        y <- claim.y until claim.y + claim.h
      } yield (x, y) -> (posToClaimCount.getOrElse((x, y), 0) + 1)
      posToClaimCount ++ newClaimCounts
    }.values.count(_ > 1)

  def nonOverlappingClaim(claims: Seq[Claim]): Option[Int] =
    claims.find(c1 => claims.forall { c2 => c1 == c2 || !overlap(c1, c2) }).map(_.id)

  private def overlap(c1: Claim, c2: Claim) =
    (c1.x until c1.x + c1.w).intersect(c2.x until c2.x + c2.w).nonEmpty &&
      (c1.y until c1.y + c1.h).intersect(c2.y until c2.y + c2.h).nonEmpty

  private def parseInput(input: Seq[String]) =
    input.map { case s"#$id @ $x,$y: ${w}x$h" => Claim(id.toInt, x.toInt, y.toInt, w.toInt, h.toInt) }

}
