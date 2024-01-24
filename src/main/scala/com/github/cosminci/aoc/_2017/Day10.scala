package com.github.cosminci.aoc._2017

import com.github.cosminci.aoc.utils

object Day10 {

  def main(args: Array[String]): Unit = {
    val input = utils.loadInputAsStrings("2017/day10.txt").head

    println(s"Part 1: ${productOfHeadElemsAfterSingleUpdate(input)}")
    println(s"Part 2: ${knotHash(input)}")
  }

  final case class State(string: Seq[Int], currPos: Int, skip: Int)

  def productOfHeadElemsAfterSingleUpdate(lengths: String): Int =
    lengths
      .split(',').map(_.toInt)
      .foldLeft(State(string = 0 to 255, currPos = 0, skip = 0))(updateState)
      .string.take(2).product

  def knotHash(input: String): String = {
    val nums       = input.map(_.toInt) ++ Seq(17, 31, 73, 47, 23)
    val state      = State(string = 0 to 255, currPos = 0, skip = 0)
    val sparseHash = (1 to 64).foldLeft(state)((s, _) => nums.foldLeft(s)(updateState)).string
    sparseHash.grouped(16).map(_.reduce(_ ^ _)).map(v => f"$v%02x").mkString
  }

  private def updateState(s: State, len: Int) = {
    val (start, end) = (s.currPos, (s.currPos + len) % s.string.size)
    val newString    = if (start <= end) patchMiddle(s, start, end) else patchEnds(s, start, end)
    State(newString, (s.currPos + len + s.skip) % s.string.size, s.skip + 1)
  }

  private def patchMiddle(s: State, start: Int, end: Int) = {
    val slice = s.string.slice(start, end)
    s.string.patch(s.currPos, slice.reverse, slice.size)
  }

  private def patchEnds(s: State, start: Int, end: Int) = {
    val sliceAtEnd         = s.string.slice(start, s.string.size)
    val sliceAtStart       = s.string.slice(0, end)
    val reversed           = (sliceAtEnd ++ sliceAtStart).reverse
    val (newEnd, newStart) = reversed.splitAt(sliceAtEnd.length)
    s.string.patch(0, newStart, newStart.length).patch(start, newEnd, newEnd.length)
  }

}
