package com.github.cosminci.aoc._2024

import com.github.cosminci.aoc.utils
import org.jgrapht.alg.clique.BronKerboschCliqueFinder
import org.jgrapht.graph._

import scala.jdk.CollectionConverters._

object Day23 {

  def main(args: Array[String]): Unit = {
    val connections = parseInput(utils.loadInputAsStrings("2024/day23.txt"))

    println(s"Part 1: ${sizeThreeGroupCount(connections)}")
    println(s"Part 2: ${lanPartyPassword(connections)}")
  }

  def sizeThreeGroupCount(connections: Seq[(String, String)]): Int = {
    val graph = connections.foldLeft(Map.empty[String, Seq[String]].withDefaultValue(Seq.empty)) {
      case (graph, (a, b)) => graph.updated(a, graph(a) :+ b).updated(b, graph(b) :+ a)
    }
    graph.keySet.flatMap { a =>
      graph(a).combinations(2).collect {
        case Seq(b, c) if graph(b).contains(c) && Seq(a, b, c).exists(_.startsWith("t")) =>
          Set(a, b, c)
      }
    }.size
  }

  def lanPartyPassword(connections: Seq[(String, String)]): String = {
    val graph = new DefaultUndirectedGraph[String, DefaultEdge](classOf[DefaultEdge])
    connections.foreach { case (a, b) =>
      graph.addVertex(a)
      graph.addVertex(b)
      graph.addEdge(a, b)
    }
    val maximalCliqueIterator = new BronKerboschCliqueFinder(graph).iterator().asScala.map(_.asScala)
    maximalCliqueIterator.maxBy(_.size).toSeq.sorted.mkString(",")
  }

  private def parseInput(lines: Seq[String]) =
    lines.map { case s"$a-$b" => (a, b) }

}
