package com.github.cosminci.aoc._2025

import com.github.cosminci.aoc.utils.loadInputAsStrings
import org.jgrapht.alg.util.UnionFind

import scala.jdk.CollectionConverters.SetHasAsJava
import scala.math.BigInt.int2bigInt

object Day8 {

  def main(args: Array[String]): Unit = {
    val (boxes, edges) = parseInput(loadInputAsStrings("2025/day8.txt"))

    println(s"Part 1: ${connectShortestNEdges(boxes, edges, n = 1000)}")
    println(s"Part 2: ${connectUntilSingleCircuit(boxes, edges)}")
  }

  final case class Pos(x: Int, y: Int, z: Int)
  final case class Edge(p1: Pos, p2: Pos)

  def connectShortestNEdges(boxes: Seq[Pos], edges: Seq[Edge], n: Int): Long = {
    val uf = new UnionFind[Pos](boxes.toSet.asJava)
    @annotation.tailrec
    def connect(edgeIdx: Int): Long =
      if (edgeIdx == n)
        boxes.groupBy(uf.find).values.map(_.size.toLong).toSeq.sorted.takeRight(3).product
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
      if (boxes.groupBy(uf.find).values.map(_.size).toSeq.max == boxes.size)
        edges(edgeIdx - 1).p1.x.toLong * edges(edgeIdx - 1).p2.x
      else {
        uf.union(edges(edgeIdx).p1, edges(edgeIdx).p2)
        connect(edgeIdx + 1)
      }
    connect(edgeIdx = 0)
  }

  private def euclidianDistance(e: Edge) =
    math.sqrt(((e.p1.x - e.p2.x).pow(2) + (e.p1.y - e.p2.y).pow(2) + (e.p1.z - e.p2.z).pow(2)).toDouble)

  private def parseInput(lines: Seq[String]) = {
    val boxes = lines.map(_.split(',').map(_.toInt)).map { case Array(x, y, z) => Pos(x, y, z) }
    val edges = boxes.combinations(2).toSeq.map { case Seq(p1, p2) => Edge(p1, p2) }
    (boxes, edges.sortBy(euclidianDistance))
  }

}
