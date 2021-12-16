package com.github.cosminci.aoc._2021

import com.github.cosminci.aoc.utils

import java.lang.{Long => JLong}
import scala.annotation.tailrec

object Day16 {
  def main(args: Array[String]): Unit = {
    val input = utils.loadInputAsStrings("2021/day16.txt").head.flatMap { char =>
      Integer.parseInt(Character.toString(char), 16).toBinaryString.reverse.padTo(4, '0').reverse
    }

    val packet = parsePacket(input.iterator)
    println(s"Part I: ${sumPacketVersions(packet)}")
    println(s"Part II: ${computeResult(packet)}")
  }

  def sumPacketVersions(packet: Packet): Int =
    packet match {
      case Literal(version, _)              => version
      case Operator(version, _, subpackets) => version + subpackets.map(sumPacketVersions).sum
    }

  def computeResult(packet: Packet): Long =
    packet match {
      case Literal(_, value) => value
      case Operator(_, opType, subpackets) =>
        val subpacketValues = subpackets.map(computeResult)
        opType match {
          case 0 => subpacketValues.sum
          case 1 => subpacketValues.product
          case 2 => subpacketValues.min
          case 3 => subpacketValues.max
          case 5 => Option.when(subpacketValues.head > subpacketValues.last)(1L).getOrElse(0L)
          case 6 => Option.when(subpacketValues.head < subpacketValues.last)(1L).getOrElse(0L)
          case _ => Option.when(subpacketValues.head == subpacketValues.last)(1L).getOrElse(0L)
        }
    }

  sealed trait Packet { def version: Int }
  case class Literal(version: Int, value: Long)                           extends Packet
  case class Operator(version: Int, opType: Int, subpackets: Seq[Packet]) extends Packet

  private def parsePacket(iterator: Iterator[Char]): Packet = {
    val version = Integer.parseInt(iterator.take(3).mkString, 2)
    val opType  = Integer.parseInt(iterator.take(3).mkString, 2)

    def operatorPackets =
      if (iterator.next() == '0') parsePackets(iterator.take(Integer.parseInt(iterator.take(15).mkString, 2)).mkString)
      else parsePackets(iterator, Integer.parseInt(iterator.take(11).mkString, 2))

    if (opType == 4) Literal(version, literalValue(iterator))
    else Operator(version, opType, operatorPackets)
  }

  private def parsePackets(bits: String): Seq[Packet] = {
    val iterator = bits.iterator
    @tailrec
    def dfs(packets: Seq[Packet]): Seq[Packet] =
      if (iterator.isEmpty) packets
      else dfs(packets :+ parsePacket(iterator))

    dfs(packets = Seq.empty)
  }

  private def parsePackets(iterator: Iterator[Char], total: Int): Seq[Packet] = {
    @tailrec
    def dfs(count: Int, packets: Seq[Packet]): Seq[Packet] =
      if (count == total) packets
      else dfs(count + 1, packets :+ parsePacket(iterator))

    dfs(count = 0, packets = Seq.empty)
  }

  private def literalValue(iterator: Iterator[Char]): Long = {
    @tailrec
    def dfs(packets: Seq[Char]): Seq[Char] = {
      val head = iterator.next()
      if (head == '0') packets ++ iterator.take(4)
      else dfs(packets ++ iterator.take(4))
    }
    JLong.parseLong(dfs(packets = Seq.empty).mkString, 2)
  }
}
