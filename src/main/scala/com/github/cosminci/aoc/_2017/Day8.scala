package com.github.cosminci.aoc._2017

import com.github.cosminci.aoc.utils

object Day8 {

  def main(args: Array[String]): Unit = {
    val ops = parseInput(utils.loadInputAsStrings("2017/day8.txt"))

    println(s"Part 1: ${maxRegisterValueAfterOps(ops)}")
    println(s"Part 2: ${maxRegisterValueDuringOps(ops)}")
  }

  def maxRegisterValueAfterOps(ops: Seq[Operation]): Int =
    registerStatesDuringOps(ops).last.values.max

  def maxRegisterValueDuringOps(ops: Seq[Operation]): Int =
    registerStatesDuringOps(ops).map(_.values.max).max

  private def registerStatesDuringOps(ops: Seq[Operation]) =
    ops.scanLeft(buildRegisters(ops)) { case (registers, op) =>
      if (!evalCond(op.condition, registers)) registers
      else op.op match {
        case "inc" => registers.updated(op.register, registers(op.register) + op.value)
        case "dec" => registers.updated(op.register, registers(op.register) - op.value)
      }
    }

  private def buildRegisters(ops: Seq[Operation]) =
    ops.flatMap(op => Seq(op.register, op.condition.register)).distinct.map(_ -> 0).toMap

  private def evalCond(condition: Condition, registers: Map[String, Int]) =
    condition.op match {
      case ">"  => registers(condition.register) > condition.value
      case "<"  => registers(condition.register) < condition.value
      case ">=" => registers(condition.register) >= condition.value
      case "<=" => registers(condition.register) <= condition.value
      case "==" => registers(condition.register) == condition.value
      case "!=" => registers(condition.register) != condition.value
    }

  final case class Condition(register: String, op: String, value: Int)
  final case class Operation(register: String, op: String, value: Int, condition: Condition)

  private def parseInput(ops: Seq[String]): Seq[Operation] = ops.map {
    case s"$register $op $value if $condRegister $condOp $condValue" =>
      Operation(register, op, value.toInt, Condition(condRegister, condOp, condValue.toInt))
  }

}
