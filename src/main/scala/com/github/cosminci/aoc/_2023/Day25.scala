package com.github.cosminci.aoc._2023

import com.github.cosminci.aoc.utils
import org.jgrapht.alg.StoerWagnerMinimumCut
import org.jgrapht.graph.{DefaultEdge, SimpleGraph}

import scala.util.chaining._

object Day25 {

  def main(args: Array[String]): Unit = {
    val input = utils.loadInputAsStrings("2023/day25.txt")

    println(s"Part 1: ${componentSizeProductAfterCuts(input)}")
  }

  def componentSizeProductAfterCuts(input: Seq[String]): Int = {
    val graph     = buildGraph(input)
    val component = new StoerWagnerMinimumCut(graph).minCut()

    component.size * (graph.vertexSet.size - component.size)
  }

  private def buildGraph(input: Seq[String]) =
    new SimpleGraph[String, DefaultEdge](classOf[DefaultEdge]).tap { graph =>
      input.foreach { case s"$u: $vs" =>
        graph.addVertex(u)
        vs.split(" ").foreach { v =>
          graph.addVertex(v)
          graph.addEdge(u, v)
        }
      }
    }

}
