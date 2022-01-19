package com.github.cosminci.aoc._2020

import com.github.cosminci.aoc.utils

object Day18 {

  def main(args: Array[String]): Unit = {
    val input = utils.loadInputAsStrings("2020/day18.txt").map(_.replace(" ", ""))

    println(s"Part 1: ${input.map(s => evalEqualPrecedence(s.iterator)).sum}")
    println(s"Part 2: ${input.map(s => evalAddHigherPrecedence(s.iterator)).sum}")
  }

  def evalEqualPrecedence(expr: Iterator[Char]): Long = {
    val (result, operand, operation) = Iterator
      .iterate(0L, 0L, '+') { case (result, operand, operation) =>
        expr.next() match {
          case d if d.isDigit   => (result, d - '0', operation)
          case op @ ('+' | '*') => (applyOperation(result, operand, operation), 0, op)
          case '('              => (applyOperation(result, evalEqualPrecedence(expr), operation), 0, '+')
          case ')'              => return applyOperation(result, operand, operation)
        }
      }
      .dropWhile(_ => expr.hasNext)
      .next()

    applyOperation(result, operand, operation)
  }

  def evalAddHigherPrecedence(expr: Iterator[Char]): Long = {
    val (stack, prevOperation) = Iterator
      .iterate((Seq.empty[Long], '_')) { case (stack, operation) =>
        expr.next() match {
          case d if d.isDigit   => (stack :+ (d - '0'), operation)
          case op @ ('+' | '*') => (evalStackHead(stack, operation), op)
          case '('              => (stack :+ evalAddHigherPrecedence(expr), operation)
          case ')'              => return evalStackHead(stack, operation).product
        }
      }
      .dropWhile(_ => expr.hasNext)
      .next()

    evalStackHead(stack, prevOperation).product
  }

  private def evalStackHead(stack: Seq[Long], prevOperation: Char) =
    if (prevOperation == '*' || stack.length < 2) stack
    else {
      val Seq(op1, op2) = stack.takeRight(2)
      stack.dropRight(2) :+ applyOperation(op1, op2, '+')
    }

  private def applyOperation(operand2: Long, operand1: Long, operation: Char) =
    operation match {
      case '+' => operand1 + operand2
      case '*' => operand1 * operand2
    }
}
