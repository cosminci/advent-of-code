package com.github.cosminci.aoc._2020

import cats.syntax.all._

object Day25 {

  def main(args: Array[String]): Unit = {
    val (pKey1, pKey2, mod) = (7573546, 17786549, 20201227)

    println(s"Part 1: ${findEncryptionKey(pKey1, pKey2, mod)}")
  }

  def findEncryptionKey(pKey1: Int, pKey2: Int, mod: Int): Long =
    findLoopSizeAndOtherKey(pKey1, pKey2, mod) match {
      case Left((loopSize, otherKey)) =>
        Iterator.iterate(1L)(v => (v * otherKey) % mod).drop(loopSize).next()
    }

  private def findLoopSizeAndOtherKey(pKey1: Int, pKey2: Int, mod: Int) =
    LazyList.from(1).foldM(1) { case (acc, loop) =>
      val newAcc = (acc * 7) % mod
      if (newAcc == pKey1) (loop, pKey2).asLeft
      else if (newAcc == pKey2) (loop, pKey1).asLeft
      else newAcc.asRight
    }
}
