package com.github.cosminci.aoc._2024

import com.github.cosminci.aoc.utils

object Day24 {

  def main(args: Array[String]): Unit = {
    val (wires, gates) = parseInput(utils.loadInputAsStrings("2024/day24.txt"))

    println(s"Part 1: ${zWireOutput(wires, gates)}")
    println(s"Part 2: ${faultyGates(gates).map(_.out).sorted.mkString(",")}")
  }

  final case class Gate(kind: String, in1: String, in2: String, out: String)

  def zWireOutput(wires: Map[String, Int], gates: Seq[Gate]): Long = {
    val outputToGate = gates.map(g => g.out -> g).toMap
    def dfs(curr: String): Int =
      wires.getOrElse(curr, outputToGate(curr) match {
        case Gate("AND", in1, in2, _) => dfs(in1) & dfs(in2)
        case Gate("OR", in1, in2, _) => dfs(in1) | dfs(in2)
        case Gate("XOR", in1, in2, _) => dfs(in1) ^ dfs(in2)
      })
    val outputWires = gates.map(_.out).filter(_.startsWith("z"))
    outputWires.sorted.map(dfs).zipWithIndex.map { case (bit, idx) => bit.toLong << idx }.sum
  }

  def faultyGates(gates: Seq[Gate]): Seq[Gate] =
    gates.filter(_.out.startsWith("z")).filter(isFaultyOutputGate(_, gates)) ++
      gates.filter(_.kind == "XOR").filter(isFaultyXORGate(_, gates)) ++
      gates.filter(_.kind == "AND").filter(isFaultyANDGate(_, gates))

  private def isFaultyOutputGate(curr: Gate, gates: Seq[Gate]) = {
    val lastOutput = gates.map(_.out).max
    curr.out != lastOutput && curr.kind != "XOR"
  }

  private def isFaultyXORGate(curr: Gate, gates: Seq[Gate]) =
    Seq(curr.in1, curr.in2, curr.out).forall(wire => !Seq('x', 'y', 'z').contains(wire.head)) ||
      gates.exists(next => next.kind == "OR" && Seq(next.in1, next.in2).contains(curr.out))

  private def isFaultyANDGate(curr: Gate, gates: Seq[Gate]) =
    !Seq(curr.in1, curr.in2).contains("x00") &&
      gates.exists(next => next.kind != "OR" && Seq(next.in1, next.in2).contains(curr.out))

  private def parseInput(lines: Seq[String]) = {
    val (wireLines, gateLines) = lines.splitAt(lines.indexWhere(_.isEmpty))
    val wires = wireLines.map { case s"$id: $signal" => (id, signal.toInt) }.toMap
    val gates = gateLines.tail.map { case s"$in1 $kind $in2 -> $out" => Gate(kind, in1, in2, out) }
    (wires, gates)
  }

}
