package com.github.cosminci.aoc._2020

object Day15 {

  def main(args: Array[String]): Unit = {
    val nums = Seq[Long](14, 8, 16, 0, 1, 17)

    println(s"Part 1: ${nthNumber(nums, 2020)}")
    println(s"Part 2: ${nthNumber(nums, 30000000)}")
  }

  def nthNumber(nums: Seq[Long], limit: Int): Long = {
    val registry = nums.zipWithIndex.map { case (n, i) => n -> Seq(i) }.toMap.withDefaultValue(Seq.empty)

    (nums.length until limit)
      .foldLeft((registry, nums.last)) { case ((registry, prev), index) =>
        val curr = registry(prev) match {
          case prevIdx1 +: prevIdx2 +: _ => prevIdx1 - prevIdx2
          case _                         => 0
        }
        (registry.updated(curr, index +: registry(curr)), curr)
      }
      ._2
  }
}
