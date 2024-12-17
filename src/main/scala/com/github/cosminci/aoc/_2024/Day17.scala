package com.github.cosminci.aoc._2024

import com.github.cosminci.aoc.utils

object Day17 {

  def main(args: Array[String]): Unit = {
    val (registers, program) = parseInput(utils.loadInputAsStrings("2024/day17.txt"))

    println(s"Part 1: ${deviceOutput(registers, program).mkString(",")}")
    println(s"Part 2: ${initialAValueForSelfOutput(registers, program)}")
  }

  final case class Registers(A: Long, B: Long, C: Long)
  final case class State(registers: Registers, pointer: Int)

  def deviceOutput(registers: Registers, program: Vector[Int]): Seq[Long] = {
    @annotation.tailrec
    def dfs(state: State, output: Seq[Long]): Seq[Long] =
      if (state.pointer >= program.length) output
      else {
        val (regs, opcode, operand) = (state.registers, program(state.pointer), program(state.pointer + 1))
        opcode match {
          case 0 => dfs(State(regs.copy(A = regs.A >> operandValue(operand, regs)), state.pointer + 2), output)
          case 1 => dfs(State(regs.copy(B = regs.B ^ operand), state.pointer + 2), output)
          case 2 => dfs(State(regs.copy(B = operandValue(operand, regs) % 8), state.pointer + 2), output)
          case 3 => dfs(State(regs, if (regs.A != 0) operand else state.pointer + 2), output)
          case 4 => dfs(State(regs.copy(B = regs.B ^ regs.C), state.pointer + 2), output)
          case 5 => dfs(state.copy(pointer = state.pointer + 2), output :+ operandValue(operand, regs) % 8)
          case 6 => dfs(State(regs.copy(B = regs.A >> operandValue(operand, regs)), state.pointer + 2), output)
          case 7 => dfs(State(regs.copy(C = regs.A >> operandValue(operand, regs)), state.pointer + 2), output)
        }
      }
    dfs(State(registers, pointer = 0), output = Seq.empty)
  }

  def initialAValueForSelfOutput(registers: Registers, program: Vector[Int]): Long =
    (1 to program.length).foldLeft(0L) { (A, n) =>
      Iterator
        .iterate(A << 3)(_ + 1)
        .dropWhile(nextA => program.takeRight(n) != deviceOutput(registers.copy(A = nextA), program))
        .next()
    }

  private def operandValue(operand: Int, registers: Registers) = operand match {
    case v if v >= 0 && v <= 3 => v
    case 4                     => registers.A
    case 5                     => registers.B
    case 6                     => registers.C
  }

  private def parseInput(lines: Seq[String]) = {
    val registers = lines.take(3).map { case s"Register $_: $value" => value.toInt }
    val program   = lines.last match { case s"Program: $values" => values.split(',').map(_.toInt) }
    Registers(registers.head, registers(1), registers(2)) -> program.toVector
  }

}
