package com.github.cosminci.aoc._2023

import com.github.cosminci.aoc.utils

import scala.collection.mutable

object Day12 {

  def main(args: Array[String]): Unit = {
    val springs = utils.loadInputAsStrings("2023/day12.txt").map(parseSprings)

    println(s"Part 1: ${waysToArrangeSprings(springs)}")
    println(s"Part 2: ${waysToArrangeSprings(springs.map(unfoldSpring))}")
  }

  final case class SpringRow(conditions: String, damagedGroups: Seq[Int])

  def waysToArrangeSprings(springs: Seq[SpringRow]): Long =
    springs.map(waysToArrangeSpring).sum

  private def waysToArrangeSpring(spring: SpringRow): Long = {
    val mem = mutable.Map.empty[(Int, Seq[Int], Boolean, Boolean), Long]
    def dfs(i: Int, damaged: Seq[Int], draining: Boolean, justDrained: Boolean): Long =
      mem.getOrElseUpdate((i, damaged, draining, justDrained), {
        if (i == spring.conditions.length) if (damaged.isEmpty) 1 else 0
        else
          spring.conditions(i) match {
            case '.' if draining    => 0
            case '.'                => dfs(i + 1, damaged, draining = false, justDrained = false)
            case '#' if justDrained => 0
            case '#'                => removeDamaged(i, damaged)
            case '?' if draining    => removeDamaged(i, damaged)
            case '?' if justDrained => dfs(i + 1, damaged, draining = false, justDrained = false)
            case '?' => dfs(i + 1, damaged, draining = false, justDrained = false) + removeDamaged(i, damaged)
          }
      })

    def removeDamaged(i: Int, groups: Seq[Int]) = groups match {
      case 1 +: tail => dfs(i + 1, tail, draining = false, justDrained = true)
      case n +: tail => dfs(i + 1, (n - 1) +: tail, draining = true, justDrained = false)
      case _ => 0
    }

    dfs(i = 0, spring.damagedGroups, draining = false, justDrained = false)
  }

  private def unfoldSpring(spring: SpringRow) =
    SpringRow(Seq.fill(5)(spring.conditions).mkString("?"), Seq.fill(5)(spring.damagedGroups).flatten)

  private def parseSprings(s: String) = {
    val (conditions, damagedGroups) = s.splitAt(s.indexOf(' '))
    SpringRow(conditions, damagedGroups.tail.split(',').map(_.toInt).toSeq)
  }

}
