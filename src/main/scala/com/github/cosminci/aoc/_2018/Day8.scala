package com.github.cosminci.aoc._2018

import cats.parse.{Numbers, Parser}
import com.github.cosminci.aoc.utils

object Day8 {

  def main(args: Array[String]): Unit = {
    val root = parseTree(utils.loadInputAsStrings("2018/day8.txt").head)

    println(s"Part 1: ${metadataValueSum(root)}")
    println(s"Part 2: ${rootValue(root)}")
  }

  final case class Header(children: Int, metadata: Int)
  final case class Node(header: Header, children: List[Node], metadata: List[Int])

  def metadataValueSum(root: Node): Int = {
    def dfs(node: Node): Int =
      node.metadata.sum + node.children.map(dfs).sum

    dfs(root)
  }

  def rootValue(root: Node): Int = {
    def dfs(node: Node): Int =
      if (node.children.isEmpty) node.metadata.sum
      else node.metadata.collect { case i if i > 0 && i <= node.children.size => dfs(node.children(i - 1)) }.sum

    dfs(root)
  }

  private val headerParser = for {
    children <- Numbers.nonNegativeIntString.map(_.toInt)
    _        <- Parser.char(' ')
    metadata <- Numbers.nonNegativeIntString.map(_.toInt)
  } yield Header(children, metadata)

  private val nodeParser = Parser.recursive[Node] { recurse =>
    for {
      h        <- headerParser
      _        <- Parser.char(' ')
      children <- recurse.repSep0(h.children, h.children, Parser.char(' '))
      _        <- if (h.children > 0) Parser.char(' ') else Parser.unit
      metadata <- Numbers.nonNegativeIntString.map(_.toInt).repSep0(h.metadata, h.metadata, Parser.char(' '))
    } yield Node(h, children, metadata)
  }

  private def parseTree(license: String) =
    nodeParser.parseAll(license).toOption.get

}
