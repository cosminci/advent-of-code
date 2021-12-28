package com.github.cosminci.aoc._2020

import com.github.cosminci.aoc.utils

object Day7 {

  def main(args: Array[String]): Unit = {
    val input = utils.loadInputAsStrings("2020/day7.txt").map(parseLine)

    println(s"Part 1: ${countBagsThatContain(input, "shiny gold")}")
    println(s"Part 2: ${countTotalBags(input.toMap, "shiny gold")}")
  }

  private val bagPattern      = "([a-z]+ [a-z]+) bag[s]?"
  private val parentPattern   = s"$bagPattern contain[s]? (.*)".r
  private val bagCountPattern = s"([0-9]+) $bagPattern[.]?".r

  def countBagsThatContain(input: Seq[(String, Array[(String, Int)])], rootBagType: String): Int = {
    val childToParent = input.foldLeft(Map.empty[String, Set[String]].withDefaultValue(Set.empty)) {
      case (acc, (parentBag, childBags)) =>
        childBags.foldLeft(acc) { case (acc, (childBag, _)) => acc.updated(childBag, acc(childBag) + parentBag) }
    }

    def dfs(bagType: String): Set[String] =
      childToParent(bagType).flatMap(dfs) + bagType

    (dfs(rootBagType) - rootBagType).size
  }

  def countTotalBags(parentToChild: Map[String, Array[(String, Int)]], rootBagType: String): Int = {
    def dfs(bagType: String): Int =
      1 + parentToChild(bagType).map { case (childBag, count) => count * dfs(childBag) }.sum

    dfs(rootBagType) - 1
  }

  private def parseLine(line: String) = line match {
    case parentPattern(parentBag, contents) =>
      parentBag -> contents.split(",").map(_.trim).collect {
        case bagCountPattern(count, childBag) =>
          childBag -> count.toInt
      }
  }
}
