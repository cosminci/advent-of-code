package com.github.cosminci.aoc._2021

import cats.syntax.either._
import com.github.cosminci.aoc.utils

import scala.annotation.tailrec

object Day10 {
  def main(args: Array[String]): Unit = {
    val input = utils.loadInputAsStrings("2021/day10.txt")

    println(s"Part I: ${syntaxErrorScore(input)}")
    println(s"Part II: ${autocompleteScore(input)}")
  }

  def syntaxErrorScore(lines: Seq[String]): Int =
    lines.flatMap(l => parseLine(l).left.toOption).sum

  def autocompleteScore(lines: Seq[String]): Long = {
    val elementScores = Map('(' -> 1, '[' -> 2, '{' -> 3, '<' -> 4)
    val scores = lines.flatMap { line =>
      parseLine(line).toOption.map { stack =>
        stack.foldRight(0L)((char, score) => score * 5 + elementScores(char))
      }
    }
    scores.sorted.apply(scores.length / 2)
  }

  private def parseLine(line: String): Either[Int, Seq[Char]] = {
    @tailrec
    def dfs(idx: Int, stack: Seq[Char]): Either[Int, Seq[Char]] =
      if (idx == line.length) stack.asRight
      else
        line(idx) match {
          case '(' | '[' | '{' | '<'    => dfs(idx + 1, stack :+ line(idx))
          case ')' if stack.last != '(' => 3.asLeft
          case ']' if stack.last != '[' => 57.asLeft
          case '}' if stack.last != '{' => 1197.asLeft
          case '>' if stack.last != '<' => 25137.asLeft
          case _                        => dfs(idx + 1, stack.dropRight(1))
        }
    dfs(idx = 0, stack = Seq.empty)
  }
}
