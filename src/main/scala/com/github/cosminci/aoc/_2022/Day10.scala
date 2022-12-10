package com.github.cosminci.aoc._2022

import com.github.cosminci.aoc.utils

import scala.util.chaining._

object Day10 {

  def main(args: Array[String]): Unit = {
    val input = utils.loadInputAsStrings("2022/day10.txt")

    val stateScan = executeOps(extractOps(input))

    println(s"Part 1: ${signalStrengthSum(stateScan)}")
    println("Part 2:").pipe(_ => renderCRT(stateScan))
  }

  def signalStrengthSum(stateScan: Seq[Int]): Int =
    Seq(20, 60, 100, 140, 180, 220)
      .map(cycle => cycle * stateScan(cycle - 1))
      .sum

  def renderCRT(stateScan: Seq[Int]): Unit =
    stateScan.zipWithIndex
      .map { case (spriteCenter, cycle) => if (spriteOverlaps(spriteCenter, cycle)) '#' else '.' }
      .grouped(40)
      .foreach(line => println(line.mkString(" ")))

  private def spriteOverlaps(spriteCenter: Int, cycle: Int) =
    Seq(-1, 0, 1).map(_ + spriteCenter).contains(cycle % 40)

  sealed trait Operation { val v: Int }
  case object Noop        extends Operation { val v = 0 }
  case class Addx(v: Int) extends Operation

  private def executeOps(ops: Seq[Operation]): Seq[Int] =
    ops.map(_.v).scanLeft(1)(_ + _)

  private def extractOps(input: Seq[String]) =
    input.flatMap {
      case "noop"         => Seq(Noop)
      case s"addx $value" => Seq(Noop, Addx(value.toInt))
    }

}
