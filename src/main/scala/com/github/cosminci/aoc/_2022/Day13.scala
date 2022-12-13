package com.github.cosminci.aoc._2022

import cats.Eq
import cats.parse.{Numbers, Parser => P}
import cats.syntax.eq._
import com.github.cosminci.aoc.utils

object Day13 {

  def main(args: Array[String]): Unit = {
    val input   = utils.loadInputAsStrings("2022/day13.txt")
    val packets = input.filter(_.nonEmpty).map(packetParser.parseAll(_).toOption.get)

    println(s"Part 1: ${countOrderedPairs(packets)}")
    println(s"Part 2: ${findDecoderKey(packets)}")
  }

  sealed trait PacketData
  case class IntData(v: Int)              extends PacketData
  case class ListData(v: Seq[PacketData]) extends PacketData

  def countOrderedPairs(packets: Seq[PacketData]): Int =
    packets
      .grouped(2)
      .zipWithIndex
      .collect { case (Seq(packet1, packet2), idx) if packetOrd.compare(packet1, packet2) < 0 => idx + 1 }
      .sum

  def findDecoderKey(packets: Seq[PacketData]): Int = {
    val dividers = Seq(2, 6).map(v => ListData(Seq(ListData(Seq(IntData(v))))))
    val sorted   = (dividers ++ packets).sorted
    dividers.map(div => sorted.indexWhere(_ === div) + 1).product
  }

  implicit val packetEq: Eq[PacketData] = Eq.fromUniversalEquals[PacketData]
  implicit val packetOrd: Ordering[PacketData] = {
    case (IntData(v1), IntData(v2))         => v1.compare(v2)
    case (ListData(Seq()), ListData(Seq())) => 0
    case (ListData(Seq()), _)               => -1
    case (_, ListData(Seq()))               => 1
    case (v1: IntData, p2: ListData)        => packetOrd.compare(ListData(Seq(v1)), p2)
    case (p1: ListData, v2: IntData)        => packetOrd.compare(p1, ListData(Seq(v2)))
    case (ListData(head1 +: tail1), ListData(head2 +: tail2)) =>
      val result = packetOrd.compare(head1, head2)
      if (result != 0) result else packetOrd.compare(ListData(tail1), ListData(tail2))
  }

  private val packetParser = P.recursive[PacketData] { recurse =>
    val intParser  = Numbers.digits.map(n => IntData(n.toInt))
    val listParser = recurse.repSep0(P.char(',')).with1.between(P.char('['), P.char(']')).map(ListData)
    intParser.orElse(listParser)
  }

}
