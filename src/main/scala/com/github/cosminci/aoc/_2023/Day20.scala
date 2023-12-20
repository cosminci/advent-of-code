package com.github.cosminci.aoc._2023

import com.github.cosminci.aoc.utils

import scala.util.chaining._

object Day20 {

  def main(args: Array[String]): Unit = {
    val (modules, circuit) = parseInput(utils.loadInputAsStrings("2023/day20.txt"))

    println(s"Part 1: ${pulseCountProduct(modules, circuit)}")
  }

  sealed trait Module
  case class FlipFlop(on: Boolean)                       extends Module
  case class Conjuction(inputs: Map[String, PulseValue]) extends Module
  case object Broadcast                                  extends Module
  case object Output                                     extends Module

  case class Pulse(src: String, dst: String, value: PulseValue)
  sealed trait PulseValue
  case object Low  extends PulseValue
  case object High extends PulseValue

  type ModuleState = Map[String, Module]
  type Circuit     = Map[String, Seq[String]]

  case class PulseCount(low: Int, high: Int) {
    def combine(other: PulseCount): PulseCount = PulseCount(low = low + other.low, high = high + other.high)
  }

  def pulseCountProduct(modules: ModuleState, circuit: Circuit): Int =
    (1 to 1000).foldLeft(modules, PulseCount(low = 0, high = 0)) { case ((modules, cnt), _) =>
      pressButton(modules, circuit).pipe { case (modules, newCnt) => (modules, cnt.combine(newCnt)) }
    }.pipe { case (_, PulseCount(low, high)) => low * high }

  private def pressButton(modules: ModuleState, circuit: Circuit): (ModuleState, PulseCount) = {
    @annotation.tailrec
    def dfs(pulses: Seq[Pulse], modules: ModuleState, cnt: PulseCount): (ModuleState, PulseCount) =
      if (pulses.isEmpty) (modules, cnt)
      else {
        val (newPulses, updatedModules) = pulses.foldLeft(Seq.empty[Pulse], modules) {
          case ((pulseAcc, modules), pulse) =>
            val (updatedModule, newPulses) = handlePulse(modules, pulse, circuit)
            (pulseAcc ++ newPulses, modules.updated(pulse.dst, updatedModule))
        }
        dfs(newPulses, updatedModules, updatePulseCount(cnt, newPulses))
      }

    dfs(pulses = Seq(Pulse("button", "brd", Low)), modules, PulseCount(low = 1, high = 0))
  }

  private def handlePulse(modules: ModuleState, pulse: Pulse, circuit: Circuit): (Module, Seq[Pulse]) =
    modules.get(pulse.dst) match {
      case Some(ff: FlipFlop)     => handleFlipFlopPulse(ff, pulse, circuit(pulse.dst))
      case Some(conj: Conjuction) => handleConjuctionPulse(conj, pulse, circuit(pulse.dst))
      case Some(Broadcast)        => (Broadcast, circuit("brd").map(out => Pulse(src = pulse.dst, out, pulse.value)))
      case None | Some(Output)    => (Output, Seq.empty)
    }

  private def handleFlipFlopPulse(ff: FlipFlop, sentPulse: Pulse, outputs: Seq[String]) =
    sentPulse.value match {
      case High => (ff, Seq.empty)
      case Low =>
        val outputValue = if (ff.on) Low else High
        (FlipFlop(on = !ff.on), outputs.map(out => Pulse(src = sentPulse.dst, out, outputValue)))
    }

  private def handleConjuctionPulse(conj: Conjuction, sentPulse: Pulse, outputs: Seq[String]) = {
    val updatedInputs = conj.inputs.updated(sentPulse.src, sentPulse.value)
    val outputValue   = if (updatedInputs.values.forall(_ == High)) Low else High
    (Conjuction(updatedInputs), outputs.map(out => Pulse(src = sentPulse.dst, out, outputValue)))
  }

  private def updatePulseCount(cnt: PulseCount, pulses: Seq[Pulse]) =
    pulses.foldLeft(cnt) {
      case (cnt, Pulse(_, _, Low))  => cnt.copy(low = cnt.low + 1)
      case (cnt, Pulse(_, _, High)) => cnt.copy(high = cnt.high + 1)
    }

  private def parseInput(input: Seq[String]) = {
    val wiring = input.map { case s"$module -> $outputs" => (module, outputs.split(", ")) }.toMap
    wiring.map {
      case (s"%$name", outputs)     => (name -> FlipFlop(on = false), name -> outputs.toSeq)
      case (s"&$name", outputs)     => (name -> Conjuction(conjunctionInputs(name, wiring)), name -> outputs.toSeq)
      case ("broadcaster", outputs) => ("brd" -> Broadcast, "brd" -> outputs.toSeq)
    }.unzip.pipe { case (modules, circuit) => (modules.toMap, circuit.toMap) }
  }

  private def conjunctionInputs(name: String, wiring: Map[String, Array[String]]) =
    wiring.collect {
      case (input, outputs) if outputs.contains(name) =>
        input.replace("%", "").replace("&", "") -> Low
    }

}
