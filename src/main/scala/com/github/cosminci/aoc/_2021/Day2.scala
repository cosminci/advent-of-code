package com.github.cosminci.aoc._2021

import com.github.cosminci.aoc.utils

object Day2 {
  def main(args: Array[String]): Unit = {
    val commands = utils
      .loadInputAsStrings("2021/day2.txt")
      .map { str =>
        val Array(command, magnitude) = str.split(" ")
        (command, magnitude.toInt)
      }
    println(s"Part I: ${finalPosition(commands)}")
    println(s"Part II: ${finalPositionWithAim(commands)}")
  }

  def finalPosition(commands: Seq[(String, Int)]): Int = {
    val (horizontalPosition, depth) = commands.foldLeft(0, 0) { case ((horizontalPosition, depth), (command, units)) =>
      command match {
        case "forward" =>
          (horizontalPosition + units, depth)
        case "down" =>
          (horizontalPosition, depth + units)
        case _ =>
          (horizontalPosition, depth - units)
      }
    }
    horizontalPosition * depth
  }

  def finalPositionWithAim(commands: Seq[(String, Int)]): Long = {
    val (horizontalPosition, depth, _) = commands.foldLeft(0, 0, 0) {
      case ((horizontalPosition, depth, aim), (command, units)) =>
        command match {
          case "forward" =>
            (horizontalPosition + units, depth + aim * units, aim)
          case "down" =>
            (horizontalPosition, depth, aim + units)
          case _ =>
            (horizontalPosition, depth, aim - units)
        }
    }
    horizontalPosition * depth
  }
}
