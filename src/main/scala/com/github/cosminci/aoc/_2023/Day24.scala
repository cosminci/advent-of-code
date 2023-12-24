package com.github.cosminci.aoc._2023

import com.github.cosminci.aoc.utils
import com.microsoft.z3._

import scala.util.chaining._

object Day24 {

  def main(args: Array[String]): Unit = {
    val stones = parseHailstones(utils.loadInputAsStrings("2023/day24.txt"))

    println(s"Part 1: ${countStoneTrajectoryIntersections(stones)}")
    println(s"Part 2: ${initialPositionOfAnnihilatorRock(stones)}")
  }

  final case class Stone(x: Long, y: Long, z: Long, dx: Int, dy: Int, dz: Int)

  def countStoneTrajectoryIntersections(stones: Seq[Stone]): Int = {
    val minCoord = 200_000_000_000_000L
    val maxCoord = minCoord * 2
    stones.combinations(2).count { case Seq(s1, s2) =>
      intersects(s1, s2).exists { case (ix, iy) => ix.min(iy) >= minCoord && ix.max(iy) <= maxCoord }
    }
  }

  def initialPositionOfAnnihilatorRock(stones: Seq[Stone]): Long = {
    val (c, solver)     = new Context().pipe(ctx => (ctx, ctx.mkSolver()))
    val Seq(x, y, z)    = Seq("x", "y", "z").map(c.mkRealConst)
    val Seq(dx, dy, dz) = Seq("dx", "dy", "dz").map(c.mkRealConst)

    stones.zipWithIndex.foreach { case (s, i) =>
      val t = c.mkRealConst(s"t$i")
      solver.add(c.mkEq(c.mkAdd(x, c.mkMul(dx, t)), c.mkAdd(c.mkReal(s.x), c.mkMul(c.mkReal(s.dx), t))))
      solver.add(c.mkEq(c.mkAdd(y, c.mkMul(dy, t)), c.mkAdd(c.mkReal(s.y), c.mkMul(c.mkReal(s.dy), t))))
      solver.add(c.mkEq(c.mkAdd(z, c.mkMul(dz, t)), c.mkAdd(c.mkReal(s.z), c.mkMul(c.mkReal(s.dz), t))))
    }

    solver.check() match {
      case Status.SATISFIABLE => Seq(x, y, z).map(v => solver.getModel.eval(v, true).toString.toLong).sum
      case _                  => -1
    }
  }

  private def intersects(h1: Stone, h2: Stone): Option[(Double, Double)] = {
    val denom = h1.dx * h2.dy - h1.dy * h2.dx
    val t     = (h1.y - h2.y) * h2.dx - (h1.x - h2.x) * h2.dy
    val u     = (h1.y - h2.y) * h1.dx - (h1.x - h2.x) * h1.dy
    Option.when(denom != 0 && (t.min(u).min(denom) >= 0 || t.max(u).max(denom) < 0)) {
      (h1.x + (t / denom.toDouble) * h1.dx, h1.y + (t / denom.toDouble) * h1.dy)
    }
  }

  private def parseHailstones(input: Seq[String]) =
    input.map { case s"$x, $y, $z @ $dx, $dy, $dz" =>
      Stone(x.toLong, y.toLong, z.toLong, dx.toInt, dy.toInt, dz.toInt)
    }

}
