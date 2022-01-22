package com.github.cosminci.aoc._2020

object Day23 {

  def main(args: Array[String]): Unit = {
    val cups = "135468729".map(_ - '0')

    println(s"Part 1: ${cupLabelsAfterPlaying(cups)}")
    println(s"Part 2: ${multiplyStarCups(cups)}")
  }

  def cupLabelsAfterPlaying(cups: Seq[Int]): String = {
    val nextPointers = cups.sliding(2).map { case Seq(i, j) => (i, j) }.toMap + (cups.last -> cups.head)

    val finalPointers = play(nextPointers, cups.head, rounds = 100)

    Iterator.iterate(1)(cup => finalPointers(cup)).slice(1, cups.length).mkString
  }

  def multiplyStarCups(cups: Seq[Int]): Long = {
    val nextPointers =
      (cups ++ (cups.max + 1 to 1_000_000))
        .sliding(2)
        .map { case Seq(i, j) => (i, j) }
        .toMap + (1_000_000 -> cups.head)

    val finalPointers = play(nextPointers, cups.head, rounds = 10_000_000)

    finalPointers(1).toLong * finalPointers(finalPointers(1))
  }

  private def play(nextPointers: Map[Int, Int], start: Int, rounds: Int) = {
    Iterator
      .iterate((nextPointers, start)) { case (nextPointers, start) =>
        val picked = Iterator.iterate(nextPointers(start))(cup => nextPointers(cup)).take(3).toSeq

        val destination = Iterator
          .iterate(start)(cup => if (cup - 1 >= 1) cup - 1 else nextPointers.size)
          .drop(1)
          .dropWhile(picked.contains)
          .next()

        val nextUpdates = Seq((start, picked.last), (picked.last, destination), (destination, start))
        val newNextPointers = nextUpdates.foldLeft(nextPointers) { case (next, (from, to)) =>
          next.updated(from, nextPointers(to))
        }

        (newNextPointers, nextPointers(picked.last))
      }
      .drop(rounds)
      .next()
      ._1
  }
}
