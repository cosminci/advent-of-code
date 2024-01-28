package com.github.cosminci.aoc._2017

import com.github.cosminci.aoc.utils

object Day20 {

  def main(args: Array[String]): Unit = {
    val particles = parseInput(utils.loadInputAsStrings("2017/day20.txt"))

    println(s"Part 1: ${particleClosestToOriginInTime(particles)}")
    println(s"Part 2: ${nonCollidingParticleCount(particles)}")
  }

  final case class Triple(x: Int, y: Int, z: Int)
  final case class Particle(id: Int, pos: Triple, vel: Triple, acc: Triple)

  def particleClosestToOriginInTime(particles: Seq[Particle]): Int = {
    val absAcc     = particles.map(p => (p, p.acc.x.abs + p.acc.y.abs + p.acc.z.abs))
    val minAbsAcc  = absAcc.map { case (_, a) => a }.min
    val candidates = absAcc.collect { case (p, absAcc) if absAcc == minAbsAcc => p }
    candidates.minBy(distanceAfterSomeTime).id
  }

  private def distanceAfterSomeTime(p: Particle) =
    Seq((p.pos.x, p.vel.x), (p.pos.y, p.vel.y), (p.pos.z, p.vel.z))
      .map { case (pos, vel) => pos + vel * 1_000_000 }
      .map(_.abs).sum

  def nonCollidingParticleCount(particles: Seq[Particle]): Int = {
    @annotation.tailrec
    def dfs(iter: Int, particles: Iterable[Particle]): Int =
      if (iter == 1000) particles.size
      else dfs(iter + 1, particles.groupBy(_.pos).values.collect { case Seq(p) => update(p) })

    dfs(iter = 0, particles)
  }

  private def update(p: Particle) = {
    val vel = Triple(p.vel.x + p.acc.x, p.vel.y + p.acc.y, p.vel.z + p.acc.z)
    val pos = Triple(p.pos.x + vel.x, p.pos.y + vel.y, p.pos.z + vel.z)
    p.copy(pos = pos, vel = vel)
  }

  private def parseInput(input: Seq[String]) =
    input.zipWithIndex.map { case (s"p=<$px,$py,$pz>, v=<$vx,$vy,$vz>, a=<$ax,$ay,$az>", i) =>
      val pos = Triple(px.toInt, py.toInt, pz.toInt)
      val vel = Triple(vx.toInt, vy.toInt, vz.toInt)
      val acc = Triple(ax.toInt, ay.toInt, az.toInt)
      Particle(i, pos, vel, acc)
    }

}
