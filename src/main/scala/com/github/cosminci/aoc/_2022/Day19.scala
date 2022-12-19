package com.github.cosminci.aoc._2022

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.syntax.parallel._
import com.github.cosminci.aoc.utils

import scala.collection.mutable

object Day19 {

  def main(args: Array[String]): Unit = {
    val input      = utils.loadInputAsStrings("2022/day19.txt")
    val blueprints = parseInput(input)

    println(s"Part 1: ${blueprintQualitySum(blueprints)}")
    println(s"Part 2: ${maxGeodeProductFromFirstThreeBlueprints(blueprints)}")
  }

  def blueprintQualitySum(blueprints: List[Blueprint]): Int =
    blueprints
      .parTraverse(b => IO.delay(b.id * maxGeodes(b, totalTime = 24)))
      .unsafeRunSync()(IORuntime.global)
      .sum

  def maxGeodeProductFromFirstThreeBlueprints(blueprints: List[Blueprint]): Int =
    blueprints.take(3)
      .parTraverse(b => IO.delay(maxGeodes(b, totalTime = 32)))
      .unsafeRunSync()(IORuntime.global)
      .product

  case class Cost(ore: Int = 0, clay: Int = 0, obsidian: Int = 0)
  case class Robot(kind: String, cost: Cost, maxNum: Int)
  case class Blueprint(id: Int, oreRobot: Robot, clayRobot: Robot, obsidianRobot: Robot, geodeRobot: Robot)

  case class State(time: Int, robots: Map[String, Int], resources: Resources)
  case class Resources(ore: Int = 0, clay: Int = 0, obsidian: Int = 0, geode: Int = 0) {
    def afford(cost: Cost): Boolean = ore >= cost.ore && clay >= cost.clay && obsidian >= cost.obsidian
    def remove(cost: Cost): Resources = Resources(ore - cost.ore, clay - cost.clay, obsidian - cost.obsidian, geode)
  }

  private def maxGeodes(blueprint: Blueprint, totalTime: Int): Int = {
    val mem = mutable.Map.empty[State, Int]
    def dfs(state: State): Int = mem.getOrElseUpdate(state,
      if (state.time == 0) state.resources.geode
      else {
        val newState = state.copy(time = state.time - 1, resources = updateResources(state, blueprint))
        if (shouldBuild(blueprint.geodeRobot, state))
          dfs(stateAfterBuild(blueprint.geodeRobot, newState))
        else
          Seq(blueprint.oreRobot, blueprint.clayRobot, blueprint.obsidianRobot)
          .filter(robot => shouldBuild(robot, state))
          .map(robot => dfs(stateAfterBuild(robot, newState))).maxOption.getOrElse(0)
          .max(dfs(newState))
      })

    dfs(State(totalTime, robots = Map("ore" -> 1, "clay" -> 0, "obsidian" -> 0, "geode" -> 0), Resources()))
  }

  private def shouldBuild(robot: Robot, state: State): Boolean =
    state.robots.getOrElse(robot.kind, 0) < robot.maxNum && state.resources.afford(robot.cost)

  private def stateAfterBuild(robot: Robot, state: State): State = {
    val newRobots = state.robots.updated(robot.kind, state.robots(robot.kind) + 1)
    State(state.time, newRobots, state.resources.remove(robot.cost))
  }

  private def updateResources(state: State, blueprint: Blueprint) = {
    val State(time, robots, resources) = state
    Resources(
      ore = (resources.ore + robots("ore")).min(blueprint.oreRobot.maxNum * (time - 1)),
      clay = (resources.clay + robots("clay")).min(blueprint.clayRobot.maxNum * (time - 1)),
      obsidian = (resources.obsidian + robots("obsidian")).min(blueprint.obsidianRobot.maxNum * (time - 1)),
      geode = resources.geode + robots("geode")
    )
  }

  private def parseInput(input: Seq[String]) = input.map {
    case s"$_ $id: $_ costs $cost1 ore. $_ costs $cost2 ore. $_ costs $cost3 ore and $cost4 clay. $_ costs $cost5 ore and $cost6 $_" =>
      Blueprint(id.toInt,
        Robot("ore", Cost(ore = cost1.toInt), maxNum = Seq(cost1.toInt, cost2.toInt, cost3.toInt, cost5.toInt).max),
        Robot("clay", Cost(ore = cost2.toInt), maxNum = cost4.toInt),
        Robot("obsidian", Cost(ore = cost3.toInt, clay = cost4.toInt), maxNum = cost6.toInt),
        Robot("geode", Cost(ore = cost5.toInt, obsidian = cost6.toInt), maxNum = Int.MaxValue)
      )
  }.toList

}
