package com.github.cosminci.aoc._2025

import com.github.cosminci.aoc.utils.loadInputAsStrings
import org.jgrapht.alg.util.UnionFind

import scala.jdk.CollectionConverters.SetHasAsJava

object Day8 {

  def main(args: Array[String]): Unit = {
    val (boxes, edges) = parseInput(loadInputAsStrings("2025/day8.txt"))

    println(s"Part 1: ${connectShortestNEdges(boxes, edges, n = 1000)}")
    println(s"Part 2: ${connectUntilSingleCircuit(boxes, edges)}")
  }

  final case class Pos(x: Long, y: Long, z: Long)
  final case class Edge(p1: Pos, p2: Pos)

  def connectShortestNEdges(boxes: Seq[Pos], edges: Seq[Edge], n: Int): Long = {
    val uf = new UnionFind[Pos](boxes.toSet.asJava)
    @annotation.tailrec
    def connect(edgeIdx: Int): Long =
      if (edgeIdx == n)
        boxes.groupMapReduce(uf.find)(_ => 1L)(_ + _).values.toSeq.sorted.takeRight(3).product
      else {
        uf.union(edges(edgeIdx).p1, edges(edgeIdx).p2)
        connect(edgeIdx + 1)
      }
    connect(edgeIdx = 0)
  }

  def connectUntilSingleCircuit(boxes: Seq[Pos], edges: Seq[Edge]): Long = {
    val uf = new UnionFind[Pos](boxes.toSet.asJava)
    @annotation.tailrec
    def connect(edgeIdx: Int): Long =
      if (uf.numberOfSets() == 1)
        edges(edgeIdx - 1).p1.x * edges(edgeIdx - 1).p2.x
      else {
        uf.union(edges(edgeIdx).p1, edges(edgeIdx).p2)
        connect(edgeIdx + 1)
      }
    connect(edgeIdx = 0)
  }

  private def euclideanDistance(e: Edge) =
    math.sqrt(Seq((e.p1.x, e.p2.x), (e.p1.y, e.p2.y), (e.p1.z, e.p2.z)).map { case (a, b) => (a - b) * (a - b) }.sum)

  private def parseInput(lines: Seq[String]) = {
    val boxes = lines.map(_.split(',').map(_.toLong)).map { case Array(x, y, z) => Pos(x, y, z) }
    val edges = boxes.combinations(2).toSeq.map { case Seq(p1, p2) => Edge(p1, p2) }
    (boxes, edges.sortBy(euclideanDistance))
  }

}
