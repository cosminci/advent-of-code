package com.github.cosminci.aoc._2018

import com.github.cosminci.aoc.utils

import scala.collection.immutable.TreeSet

object Day7 {

  def main(args: Array[String]): Unit = {
    val graph = parseInput(utils.loadInputAsStrings("2018/day7.txt"))

    println(s"Part 1: ${assembleSleigh(graph, workers = 1).taskOrder}")
    println(s"Part 2: ${assembleSleigh(graph, workers = 5).time}")
  }

  final case class AssemblyResult(taskOrder: String, time: Int)

  def assembleSleigh(parentToChildren: Map[Char, Seq[Char]], workers: Int): AssemblyResult = {
    val childToParents = reverseGraph(parentToChildren)

    def newTasksToQueue(justCompleted: Iterable[Char], completed: Seq[Char]) =
      justCompleted.flatMap(parentToChildren).filter(childToParents(_).forall(completed.contains))

    @annotation.tailrec
    def dfs(inProgress: Map[Char, Int], queue: TreeSet[Char], completed: Seq[Char], time: Int): AssemblyResult =
      if (inProgress.isEmpty && queue.isEmpty) AssemblyResult(completed.mkString, time)
      else {
        val timeToFirstCompletion   = inProgress.values.minOption.getOrElse(0)
        val (stillInProgress, done) = completeTasks(inProgress, timeToFirstCompletion)
        val newCompleted            = completed ++ done
        val newQueue                = queue ++ newTasksToQueue(done, newCompleted)
        val numTasksToStart         = workers - stillInProgress.size
        val newToStart              = newQueue.take(numTasksToStart).map(task => task -> (61 + task - 'A'))
        val newInProgress           = (stillInProgress ++ newToStart).toMap
        dfs(newInProgress, newQueue.drop(numTasksToStart), newCompleted, time + timeToFirstCompletion)
      }

    val roots = parentToChildren.keys.filterNot(childToParents.contains)
    dfs(inProgress = Map.empty, queue = TreeSet.from(roots), completed = Seq.empty, time = 0)
  }

  private def completeTasks(tasksInProgress: Map[Char, Int], timePassed: Int) =
    tasksInProgress.partitionMap { case (task, timeLeft) =>
      Either.cond(timeLeft == timePassed, task, task -> (timeLeft - timePassed))
    }

  private def reverseGraph(parentToChildren: Map[Char, Seq[Char]]) =
    parentToChildren.foldLeft(Map.empty[Char, Seq[Char]].withDefaultValue(Seq.empty)) {
      case (graph, (parent, children)) =>
        children.foldLeft(graph)((graph, child) => graph.updated(child, graph(child) :+ parent))
    }

  private def parseInput(input: Seq[String]) =
    input.foldLeft(Map.empty[Char, Seq[Char]].withDefaultValue(Seq.empty)) {
      case (graph, s"Step $from must be finished before step $to can begin.") =>
        graph.updated(from.head, graph(from.head) :+ to.head)
    }

}
