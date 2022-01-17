package com.github.cosminci.aoc._2020

import com.github.cosminci.aoc.utils

import scala.annotation.tailrec

object Day12 {

  def main(args: Array[String]): Unit = {
    val commands = utils.loadInputAsStrings("2020/day12.txt").map(str => (str.head, str.tail.toInt))

    println(s"Part 1: ${distanceToOrigin(commands)}")
    println(s"Part 2: ${distanceToOrigin2(commands)}")
  }

  def distanceToOrigin(commands: Seq[(Char, Int)]): Int = {
    val (x, y, _) = commands.foldLeft(0, 0, 90) { case ((x, y, angle), (command, amplitude)) =>
      command match {
        case 'N' => (x, y + amplitude, angle)
        case 'S' => (x, y - amplitude, angle)
        case 'W' => (x - amplitude, y, angle)
        case 'E' => (x + amplitude, y, angle)
        case 'L' => (x, y, math.floorMod(angle - amplitude, 360))
        case 'R' => (x, y, math.floorMod(angle + amplitude, 360))
        case 'F' =>
          if (angle == 0) (x, y + amplitude, angle)
          else if (angle == 90) (x + amplitude, y, angle)
          else if (angle == 180) (x, y - amplitude, angle)
          else (x - amplitude, y, angle)
      }
    }
    math.abs(x) + math.abs(y)
  }

  def distanceToOrigin2(numbers: Seq[(Char, Int)]): Int = {
    val (x, y, _, _) = numbers.foldLeft(0, 0, 10, 1) { case ((x, y, wx, wy), (command, amplitude)) =>
      command match {
        case 'N' => (x, y, wx, wy + amplitude)
        case 'S' => (x, y, wx, wy - amplitude)
        case 'W' => (x, y, wx - amplitude, wy)
        case 'E' => (x, y, wx + amplitude, wy)
        case 'L' => rotateLeft(x, y, wx, wy, amplitude / 90)
        case 'R' => rotateRight(x, y, wx, wy, amplitude / 90)
        case 'F' => (x + amplitude * wx, y + amplitude * wy, wx, wy)
      }
    }
    math.abs(x) + math.abs(y)
  }

  @tailrec
  private def rotateLeft(x: Int, y: Int, wx: Int, wy: Int, times: Int): (Int, Int, Int, Int) =
    if (times == 0) (x, y, wx, wy)
    else rotateLeft(x, y, -wy, wx, times - 1)

  @tailrec
  private def rotateRight(x: Int, y: Int, wx: Int, wy: Int, times: Int): (Int, Int, Int, Int) =
    if (times == 0) (x, y, wx, wy)
    else rotateRight(x, y, wy, -wx, times - 1)
}
