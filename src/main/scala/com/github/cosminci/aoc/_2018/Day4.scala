package com.github.cosminci.aoc._2018

import cats.syntax.foldable._
import com.github.cosminci.aoc.utils

import scala.util.chaining._

object Day4 {

  def main(args: Array[String]): Unit = {
    val schedule = parseInput(utils.loadInputAsStrings("2018/day4.txt"))

    println(s"Part 1: ${firstStrategyResult(schedule)}")
    println(s"Part 2: ${secondStrategyResult(schedule)}")
  }

  def firstStrategyResult(schedule: Map[Int, Seq[Int]]): Int = {
    val (sleepiestGuard, guardSchedule) =
      schedule.maxBy { case (_, events) =>
        events.grouped(2).map { case Seq(start, end) => end - start }.sum
      }

    val sleepiestMinute = sleepiestMinuteInSchedule(guardSchedule).pipe { case (minute, _) => minute }

    sleepiestGuard * sleepiestMinute
  }

  def secondStrategyResult(schedule: Map[Int, Seq[Int]]): Int = {
    val (sleepiestGuard, guardSchedule) = schedule
      .maxBy { case (_, events) =>
        sleepiestMinuteInSchedule(events).pipe { case (_, count) => count }
      }

    sleepiestGuard * sleepiestMinuteInSchedule(guardSchedule).pipe { case (minute, _) => minute }
  }

  private def sleepiestMinuteInSchedule(guardSchedule: Seq[Int]) =
    guardSchedule
      .grouped(2)
      .map { case Seq(start, end) => (start until end).groupMapReduce(identity)(_ => 1)(_ + _) }.toList
      .combineAll
      .maxBy { case (_, count) => count }

  private def parseInput(input: Seq[String]) =
    input
      .sortBy { case s"[$datetime] $_" => datetime }
      .foldLeft(-1, Map.empty[Int, Seq[Int]].withDefaultValue(Seq.empty)) { case ((guard, schedule), event) =>
        event match {
          case s"[$_] Guard #${id} begins shift" => (id.toInt, schedule)
          case s"[$_:$minute] falls asleep" =>
            (guard, schedule.updated(guard, schedule(guard) :+ minute.toInt))
          case s"[$_:$minute] wakes up" =>
            (guard, schedule.updated(guard, schedule(guard) :+ minute.toInt))
        }
      }.pipe { case (_, schedule) => schedule }

}
