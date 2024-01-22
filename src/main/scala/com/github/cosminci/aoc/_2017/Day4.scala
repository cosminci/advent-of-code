package com.github.cosminci.aoc._2017

import com.github.cosminci.aoc.utils

object Day4 {

  def main(args: Array[String]): Unit = {
    val passphrases = utils.loadInputAsStrings("2017/day4.txt").map(_.split(' ').toSeq)

    println(s"Part 1: ${countValidPassphrases(passphrases, groupFn = identity)}")
    println(s"Part 2: ${countValidPassphrases(passphrases, groupFn = _.toSet)}")
  }

  private def countValidPassphrases[A](passphrases: Seq[Seq[String]], groupFn: String => A): Int =
    passphrases.count(p => p.length == p.groupMapReduce(groupFn)(_ => 1)(_ + _).size)

}
