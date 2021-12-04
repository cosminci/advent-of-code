package com.github.cosminci.aoc._2021

import com.github.cosminci.aoc.utils

import scala.util.Try

object Day4 {
  def main(args: Array[String]): Unit = {
    val input = utils.loadInputAsStrings("2021/day4.txt")
    val nums  = input.head.split(',').map(_.toInt)

    println(winningBoardScore(nums, loadBoards(input)))
    println(lastWinningBoard(nums, loadBoards(input)))
  }

  private def loadBoards(input: Seq[String]) = {
    input.tail
      .filter(_.nonEmpty)
      .map(_.split(' ').flatMap(s => Try(s.toInt).toOption).toArray)
      .grouped(5)
      .toSeq
  }

  def winningBoardScore(nums: Seq[Int], boards: Seq[Seq[Array[Int]]]): Int = {
    val boardPositions = positions(boards)

    nums.foreach { num =>
      boardPositions(num).foreach { case (boardIdx, rowIdx, colIdx) =>
        boards(boardIdx)(rowIdx)(colIdx) = -1
        if (boards(boardIdx)(rowIdx).forall(_ == -1) || boards(boardIdx).map(_(colIdx)).forall(_ == -1))
          return score(num, boards(boardIdx))
      }
    }

    0 // no board wins
  }

  def lastWinningBoard(nums: Seq[Int], boards: Seq[Seq[Array[Int]]]): Int = {
    val boardPositions = positions(boards)

    nums.foldLeft(Set.from(boards.indices)) { (boardsLeft, num) =>
      boardPositions(num)
        .filter { case (boardIdx, _, _) => boardsLeft.contains(boardIdx) }
        .foldLeft(boardsLeft) { case (boardsLeft, (boardIdx, rowIdx, colIdx)) =>
          boards(boardIdx)(rowIdx)(colIdx) = -1
          if (boards(boardIdx)(rowIdx).forall(_ == -1) || boards(boardIdx).map(_(colIdx)).forall(_ == -1)) {
            if (boardsLeft.size == 1)
              return score(num, boards(boardIdx))
            boardsLeft - boardIdx
          } else boardsLeft
        }
    }

    0 // no board wins
  }

  private def score(num: Int, board: Seq[Array[Int]]): Int =
    num * board.foldLeft(0)((sum, row) => sum + row.filterNot(_ == -1).sum)

  private def positions(boards: Seq[Seq[Array[Int]]]) = {
    boards.indices
      .foldLeft(Map.empty[Int, Seq[(Int, Int, Int)]].withDefaultValue(Seq.empty)) { case (boardPositions, boardIdx) =>
        val positions = for {
          rowIdx <- boards(boardIdx).indices
          colIdx <- boards(boardIdx)(rowIdx).indices
        } yield (boards(boardIdx)(rowIdx)(colIdx), (boardIdx, rowIdx, colIdx))
        positions.foldLeft(boardPositions) { case (boardPositions, (num, (boardIdx, rowIdx, colIdx))) =>
          boardPositions.updated(num, boardPositions(num) :+ (boardIdx, rowIdx, colIdx))
        }
      }
  }
}
