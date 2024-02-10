package com.github.cosminci.aoc._2018

import com.github.cosminci.aoc._2018.Day16.OpCode.OpCode
import com.github.cosminci.aoc.utils

object Day16 {

  def main(args: Array[String]): Unit = {
    val samples      = parseSamples(utils.loadInputAsStrings("2018/day16_samples.txt"))
    val instructions = parseInstructions(utils.loadInputAsStrings("2018/day16_program.txt"))

    println(s"Part 1: ${samplesWithAtLeastThreeOpcodes(samples)}")
    println(s"Part 2: ${valueOfFirstRegisterAfterProgramRuns(samples, instructions)}")
  }

  def samplesWithAtLeastThreeOpcodes(samples: Seq[Sample]): Int =
    samples.count(sample => OpCode.values.count(isValid(_, sample)) >= 3)

  def valueOfFirstRegisterAfterProgramRuns(samples: Seq[Sample], instructions: Seq[GenericOp]): Int = {
    val registers = Seq.fill(4)(0)
    val opCodeMap = mapOpCodes(samples)
    instructions.foldLeft(registers)((r, i) => execute(opCodeMap(i.code), i.args, r)).head
  }

  object OpCode extends Enumeration {
    type OpCode = Value
    val addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr = Value
  }

  final case class OpArgs(a: Int, b: Int, c: Int)
  final case class GenericOp(code: Int, args: OpArgs)
  final case class Op(code: OpCode, a: Int, b: Int, c: Int)
  final case class Sample(before: Seq[Int], op: GenericOp, after: Seq[Int])

  private def mapOpCodes(samples: Seq[Sample]) =
    samples.foldLeft(Map.empty[Int, Set[OpCode]]) { (map, sample) =>
      val candidates = OpCode.values.filter(isValid(_, sample))
      val code = sample.op.code
      val newMap = map.updated(code, map.getOrElse(code, OpCode.values).intersect(candidates))
      if (newMap(code).size > 1) newMap
      else newMap.view.mapValues(v => if (v.size > 1) v.excl(newMap(code).head) else v).toMap
    }.view.mapValues(_.head).toMap

  private def execute(code: OpCode, args: OpArgs, registers: Seq[Int]) =
    code match {
      case OpCode.addr => registers.updated(args.c, registers(args.a) + registers(args.b))
      case OpCode.addi => registers.updated(args.c, registers(args.a) + args.b)
      case OpCode.mulr => registers.updated(args.c, registers(args.a) * registers(args.b))
      case OpCode.muli => registers.updated(args.c, registers(args.a) * args.b)
      case OpCode.banr => registers.updated(args.c, registers(args.a) & registers(args.b))
      case OpCode.bani => registers.updated(args.c, registers(args.a) & args.b)
      case OpCode.borr => registers.updated(args.c, registers(args.a) | registers(args.b))
      case OpCode.bori => registers.updated(args.c, registers(args.a) | args.b)
      case OpCode.setr => registers.updated(args.c, registers(args.a))
      case OpCode.seti => registers.updated(args.c, args.a)
      case OpCode.gtir => registers.updated(args.c, if (args.a > registers(args.b)) 1 else 0)
      case OpCode.gtri => registers.updated(args.c, if (registers(args.a) > args.b) 1 else 0)
      case OpCode.gtrr => registers.updated(args.c, if (registers(args.a) > registers(args.b)) 1 else 0)
      case OpCode.eqir => registers.updated(args.c, if (args.a == registers(args.b)) 1 else 0)
      case OpCode.eqri => registers.updated(args.c, if (registers(args.a) == args.b) 1 else 0)
      case OpCode.eqrr => registers.updated(args.c, if (registers(args.a) == registers(args.b)) 1 else 0)
    }

  private def isValid(code: OpCode, sample: Sample) =
    execute(code, sample.op.args, sample.before) == sample.after

  private def parseSamples(input: Seq[String]) =
    input.grouped(4).map { case Seq(before, instr, after, _) =>
      val registersBefore = before match { case s"Before: [${regs}]" => regs.split(", ").map(_.toInt) }
      val registersAfter  = after match { case s"After:  [${regs}]" => regs.split(", ").map(_.toInt) }
      val op = instr.split(' ').map(_.toInt) match { case Array(code, a, b, c) => GenericOp(code, OpArgs(a, b, c)) }
      Sample(registersBefore, op, registersAfter)
    }.toSeq

  private def parseInstructions(input: Seq[String]) =
    input.map { case s"$code $a $b $c" => GenericOp(code.toInt, OpArgs(a.toInt, b.toInt, c.toInt)) }

}
