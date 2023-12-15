package com.github.cosminci.aoc._2023

import cats.parse.{Parser, Rfc5234}
import com.github.cosminci.aoc.utils

object Day15 {

  def main(args: Array[String]): Unit = {
    val initSeq = utils.loadInputAsStrings("2023/day15.txt").head.split(',').toSeq
    val initOps = parseInitOps(initSeq)

    println(s"Part 1: ${hashSum(initSeq)}")
    println(s"Part 2: ${focusingPower(initOps)}")
  }

  sealed trait Operation
  final case class InstallLens(lens: Lens)   extends Operation
  final case class RemoveLens(label: String) extends Operation

  final case class Lens(label: String, focalLength: Int)

  def hashSum(initSeq: Seq[String]): Int =
    initSeq.map(hash).sum

  def focusingPower(initOps: Seq[Operation]) =
    executeOps(initOps).map { case (box, lenses) =>
      (box + 1) * lenses.zipWithIndex.map { case (lens, slot) => lens.focalLength * (slot + 1) }.sum
    }.sum

  private def executeOps(initOps: Seq[Operation]) =
    initOps.foldLeft(Map.empty[Int, Seq[Lens]].withDefaultValue(Seq.empty)) {
      case (boxes, InstallLens(lens)) =>
        boxes.updated(hash(lens.label), installLens(lens, boxes(hash(lens.label))))
      case (boxes, RemoveLens(label)) =>
        boxes.updated(hash(label), removeLens(label, boxes(hash(label))))
    }

  private def installLens(lens: Lens, box: Seq[Lens]) =
    box.zipWithIndex.collectFirst { case (l, i) if l.label == lens.label => i } match {
      case Some(i) => box.updated(i, lens)
      case None    => box :+ lens
    }

  private def removeLens(label: String, box: Seq[Lens]) =
    box.zipWithIndex.collectFirst { case (l, i) if l.label == label => i } match {
      case Some(i) => box.patch(i, Nil, 1)
      case None    => box
    }

  private def hash(s: String) =
    s.foldLeft(0)((acc, ch) => (acc + ch) * 17 % 256)

  private def parseInitOps(initSeq: Seq[String]) = {
    val label       = Rfc5234.alpha.rep.string
    val focalLength = Rfc5234.digit.rep.string.map(_.toInt)
    val install     = ((label <* Parser.char('=')) ~ focalLength).map { case (l, fl) => InstallLens(Lens(l, fl)) }
    val remove      = (label <* Parser.char('-')).map(RemoveLens)
    initSeq.flatMap(op => install.backtrack.orElse(remove).parseAll(op).toOption)
  }

}
