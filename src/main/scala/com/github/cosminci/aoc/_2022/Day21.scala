package com.github.cosminci.aoc._2022

import com.github.cosminci.aoc.utils

object Day21 {

  def main(args: Array[String]): Unit = {
    val input         = utils.loadInputAsStrings("2022/day21.txt")
    val topDownGraph  = parseGraphTopDown(input)
    val bottomUpGraph = parseGraphBottomUp(input)

    println(s"Part 1: ${findMonkeyNumber("root", topDownGraph)}")
    println(s"Part 2: ${findOwnNumber(topDownGraph, bottomUpGraph)}")
  }

  sealed trait Equation
  case class Num(value: Long)                                    extends Equation
  case class Op(operand1: Node, operand2: Node, operation: Char) extends Equation

  sealed trait Node { def name: String }
  case class Parent(name: String)  extends Node
  case class Sibling(name: String) extends Node

  def findMonkeyNumber(name: String, graph: Map[String, Equation]): Long = {
    def dfs(node: String): Long =
      graph(node) match {
        case Num(value)          => value
        case Op(left, right, op) => applyOp(dfs(left.name), dfs(right.name), op)
      }
    dfs(name)
  }

  def findOwnNumber(topDownGraph: Map[String, Equation], bottomUpGraph: Map[String, Op]): Long = {
    def dfs(curr: String): Long =
      bottomUpGraph(curr) match {
        case Op(Parent("root"), sibling, _) => findMonkeyNumber(sibling.name, topDownGraph)
        case Op(Parent(p), Sibling(s), op)  => applyOp(dfs(p), findMonkeyNumber(s, topDownGraph), op)
        case Op(Sibling(s), Parent(p), op)  => applyOp(findMonkeyNumber(s, topDownGraph), dfs(p), op)
      }
    dfs("humn")
  }

  private def applyOp(v1: Long, v2: Long, op: Char): Long = op match {
    case '+' => v1 + v2
    case '-' => v1 - v2
    case '*' => v1 * v2
    case _   => v1 / v2
  }

  private def parseGraphTopDown(input: Seq[String]) =
    input.map {
      case s"$p: $l $op $r" => p -> Op(Sibling(l), Sibling(r), op.head)
      case s"$p: $value"    => p -> Num(value.toLong)
    }.toMap

  private def parseGraphBottomUp(input: Seq[String]) =
    input.flatMap {
      case s"$p: $l + $r" => Seq(l -> Op(Parent(p), Sibling(r), '-'), r -> Op(Parent(p), Sibling(l), '-'))
      case s"$p: $l - $r" => Seq(l -> Op(Parent(p), Sibling(r), '+'), r -> Op(Sibling(l), Parent(p), '-'))
      case s"$p: $l * $r" => Seq(l -> Op(Parent(p), Sibling(r), '/'), r -> Op(Parent(p), Sibling(l), '/'))
      case s"$p: $l / $r" => Seq(l -> Op(Parent(p), Sibling(r), '*'), r -> Op(Sibling(l), Parent(p), '/'))
      case _              => Seq.empty
    }.toMap

}
