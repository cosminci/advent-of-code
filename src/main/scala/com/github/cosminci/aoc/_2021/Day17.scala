package com.github.cosminci.aoc._2021

object Day17 {
  def main(args: Array[String]): Unit = {
    val (left, right, bottom, top) = (236, 262, -78, -58)

    println(s"Part I: ${heightsReached(left, right, bottom, top).max}")
    println(s"Part II: ${heightsReached(left, right, bottom, top).size}")
  }

  def heightsReached(left: Int, right: Int, bottom: Int, top: Int): Seq[Int] = {
    def reachesTarget(dx: Int, dy: Int): Boolean =
      Iterator
        .iterate(0, 0, dx, dy) { case (x, y, dx, dy) => (x + dx, y + dy, (dx - 1).max(0), dy - 1) }
        .takeWhile { case (x, y, _, _) => x <= right && y >= bottom }
        .exists { case (x, y, _, _) => x >= left && x <= right && y >= bottom && y <= top }

    val minDx = Iterator.iterate(0)(_ + 1).dropWhile(x => x * (x + 1) / 2 < left).next()

    for {
      dx <- minDx to right
      dy <- bottom to -bottom
      if reachesTarget(dx, dy)
    } yield dy * (dy + 1) / 2
  }
}
