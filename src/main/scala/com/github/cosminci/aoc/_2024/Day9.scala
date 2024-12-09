package com.github.cosminci.aoc._2024

import com.github.cosminci.aoc.utils

import scala.collection.immutable.{TreeMap, TreeSet}
import scala.util.chaining.scalaUtilChainingOps

object Day9 {

  def main(args: Array[String]): Unit = {
    val (used, free) = parseInput(utils.loadInputAsStrings("2024/day9.txt").head)

    println(s"Part 1: ${checksumAfterPerBlockCompaction(used, free)}")
    println(s"Part 2: ${checksumAfterPerFileCompaction(used, free)}")
  }

  def checksumAfterPerBlockCompaction(used: TreeMap[Range, Int], free: TreeSet[Range]): Long = {
    @annotation.tailrec
    def dfs(used: TreeMap[Int, Int], free: TreeSet[Int]): Long = {
      val ((lastUsedIdx, fileId), firstFreeIdx) = (used.last, free.head)
      if (lastUsedIdx <= firstFreeIdx) used.map { case (idx, fileId) => idx.toLong * fileId }.sum
      else dfs(used.removed(lastUsedIdx).updated(firstFreeIdx, fileId), free - firstFreeIdx)
    }
    dfs(used.flatMap { case (range, fileId) => range.map(_ -> fileId) }, free.flatMap(identity))
  }

  def checksumAfterPerFileCompaction(used: TreeMap[Range, Int], free: TreeSet[Range]): Long = {
    @annotation.tailrec
    def dfs(candidates: TreeSet[Range], used: TreeMap[Range, Int], free: TreeSet[Range]): Long =
      candidates.lastOption match {
        case None => used.map { case (range, fileId) => fileId.toLong * range.sum }.sum
        case Some(candidate) =>
          free.find(f => f.size >= candidate.size && f.start < candidate.start) match {
            case None => dfs(candidates.dropRight(1), used, free)
            case Some(freeRange) =>
              val (toUse, stillFree) = freeRange.splitAt(candidate.size)
              val newUsed            = used.removed(candidate).updated(toUse, used(candidate))
              val newFree            = (free - freeRange) ++ Option.when(stillFree.nonEmpty)(stillFree)
              dfs(candidates.dropRight(1), newUsed, newFree)
          }
      }
    dfs(used.keySet, used, free)
  }

  implicit val rangeOrdering: Ordering[Range] = Ordering.by(_.start)
  private def parseInput(diskMap: String) = {
    (diskMap :+ "0").map(_.toString.toInt).grouped(2)
      .foldLeft((TreeMap.empty[Range, Int], TreeSet.empty[Range], 0, 0)) {
        case ((used, free, fileId, idx), Seq(blockSize, freeSpaceSize)) =>
          val file  = (idx until idx + blockSize) -> fileId
          val empty = 0 + idx + blockSize until freeSpaceSize + idx + blockSize
          (used + file, free + empty, fileId + 1, idx + blockSize + freeSpaceSize)
      }.pipe { case (idxToFileId, freeSpace, _, _) => (idxToFileId, freeSpace.filter(_.nonEmpty)) }
  }

}
