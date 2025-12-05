package com.github.cosminci.aoc._2017

import cats.parse.Parser._
import com.github.cosminci.aoc.utils

object Day9 {

  def main(args: Array[String]): Unit = {
    val dataStream = parseInput(utils.loadInputAsStrings("2017/day9.txt").head)

    println(s"Part 1: ${groupScore(dataStream)}")
    println(s"Part 2: ${totalGarbage(dataStream)}")
  }

  sealed trait StreamItem
  final case class Group(items: Seq[StreamItem]) extends StreamItem
  final case class Garbage(length: Int)          extends StreamItem

  def groupScore(dataStream: StreamItem): Int = {
    def dfs(item: StreamItem, level: Int): Int =
      item match {
        case Garbage(_)   => 0
        case Group(items) => level + items.map(dfs(_, level + 1)).sum
      }
    dfs(item = dataStream, level = 1)
  }

  def totalGarbage(dataStream: StreamItem): Int = {
    def dfs(item: StreamItem): Int =
      item match {
        case Garbage(length) => length
        case Group(items)    => items.map(dfs).sum
      }
    dfs(item = dataStream)
  }

  private def parseInput(s: String): StreamItem = {
    val parser = recursive[StreamItem] { recurse =>
      val groupParser    = recurse.repSep0(char(',')).with1.between(char('{'), char('}')).map(Group)
      val garbageContent = ((char('!') *> anyChar).map(_ => 0) | charWhere(_ != '>').map(_ => 1)).rep0.map(_.sum)
      val garbageParser  = (char('<') *> garbageContent <* char('>')).map(Garbage)
      groupParser.orElse(garbageParser)
    }
    parser.parseAll(s).toOption.get
  }

}
