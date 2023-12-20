package com.github.cosminci.aoc._2023

import com.github.cosminci.aoc.utils

import scala.util.chaining._

object Day20 {

  def main(args: Array[String]): Unit = {
    val (modules, circuit) = parseInput(utils.loadInputAsStrings("2023/day20.txt"))

    println(s"Part 1: ${pulseCountProduct(modules, circuit)}")
    println(s"Part 2: ${buttonPressesToStartMachine(modules, circuit)}")
  }

  sealed trait Module
  final case class FlipFlop(on: Boolean)                       extends Module
  final case class Conjuction(inputs: Map[String, PulseValue]) extends Module
  final case object Broadcast                                  extends Module
  final case object Output                                     extends Module

  sealed trait PulseValue
  final case object Low  extends PulseValue
  final case object High extends PulseValue
  final case class Pulse(src: String, dst: String, value: PulseValue)

  type ModuleState = Map[String, Module]
  type Circuit     = Map[String, Seq[String]]
  final case class State(modules: ModuleState, pulses: Seq[Pulse])

  def pulseCountProduct(modules: ModuleState, circuit: Circuit): Int =
    (1 to 1000).foldLeft(modules, Seq.empty[Pulse]) { case ((modules, pulses), _) =>
      val state = pressButton(modules, circuit)
      (state.modules, pulses ++ state.pulses)
    }.pipe { case (_, pulses) => calculatePulseProduct(pulses) }

  def buttonPressesToStartMachine(modules: ModuleState, circuit: Circuit): Long = {
    val destinationToSources = reverseCircuit(circuit)
    destinationToSources("rx").flatMap(destinationToSources) // ¯\_(ツ)_/¯
      .map(buttonPressesUntilHigh(modules, circuit, _))
      .reduce(utils.lcm)
  }

  private def buttonPressesUntilHigh(modules: ModuleState, circuit: Circuit, target: String) = {
    @annotation.tailrec
    def dfs(state: State, cnt: Long): Long =
      if (state.pulses.exists(p => p.src == target && p.value == High)) cnt
      else dfs(pressButton(state.modules, circuit), cnt + 1)

    dfs(state = State(modules, pulses = Seq.empty), cnt = 0)
  }

  private def pressButton(modules: ModuleState, circuit: Circuit) = {
    @annotation.tailrec
    def dfs(state: State, allPulses: Seq[Pulse]): State =
      if (state.pulses.isEmpty) State(state.modules, allPulses)
      else {
        val updatedState = handlePulses(state, circuit)
        dfs(updatedState, allPulses ++ updatedState.pulses)
      }

    val initialState = State(modules, Seq(Pulse(src = "button", dst = "broadcaster", value = Low)))
    dfs(initialState, allPulses = initialState.pulses)
  }

  private def handlePulses(state: State, circuit: Circuit) =
    state.pulses.foldLeft(State(state.modules, Seq.empty[Pulse])) { case (State(modules, pulseAcc), pulse) =>
      val (updatedModule, newPulses) = handlePulse(modules, pulse, circuit)
      State(modules.updated(pulse.dst, updatedModule), pulseAcc ++ newPulses)
    }

  private def handlePulse(modules: ModuleState, pulse: Pulse, circuit: Circuit) =
    (modules(pulse.dst), pulse.value) match {
      case (Output, _) => (Output, Seq.empty)
      case (Broadcast, pulseValue) =>
        (Broadcast, circuit("broadcaster").map(out => Pulse(src = pulse.dst, out, pulseValue)))
      case (module: FlipFlop, High) => (module, Seq.empty)
      case (module: FlipFlop, Low) =>
        val generatedPulses = circuit(pulse.dst).map(out => Pulse(src = pulse.dst, out, if (module.on) Low else High))
        (FlipFlop(on = !module.on), generatedPulses)
      case (module: Conjuction, pulseValue) =>
        val updatedInputs = module.inputs.updated(pulse.src, pulseValue)
        val outputValue   = if (updatedInputs.values.forall(_ == High)) Low else High
        (Conjuction(updatedInputs), circuit(pulse.dst).map(out => Pulse(src = pulse.dst, out, outputValue)))
    }

  private def calculatePulseProduct(pulses: Seq[Pulse]) =
    pulses.foldLeft(0, 0) {
      case ((low, high), Pulse(_, _, Low))  => (low + 1, high)
      case ((low, high), Pulse(_, _, High)) => (low, high + 1)
    }.pipe { case (low, high) => low * high }

  private def reverseCircuit(circuit: Circuit) =
    circuit.toSeq
      .flatMap { case (src, dsts) => dsts.map(_ -> src) }
      .groupMap { case (dst, _) => dst } { case (_, src) => src }

  private def parseInput(input: Seq[String]) = {
    val wiring = input.map { case s"$module -> $outputs" => (module, outputs.split(", ")) }.toMap
    wiring.map {
      case (s"%$name", outputs)     => (name -> FlipFlop(on = false), name -> outputs.toSeq)
      case (s"&$name", outputs)     => (name -> Conjuction(conjunctionInputs(name, wiring)), name -> outputs.toSeq)
      case ("broadcaster", outputs) => ("broadcaster" -> Broadcast, "broadcaster" -> outputs.toSeq)
    }.unzip.pipe { case (modules, circuit) => (modules.toMap + ("rx" -> Output), circuit.toMap) }
  }

  private def conjunctionInputs(name: String, wiring: Map[String, Array[String]]) =
    wiring.collect { case (input, outputs) if outputs.contains(name) =>
      input.replace("%", "").replace("&", "") -> Low
    }

}
