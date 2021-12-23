package com.github.cosminci.aoc._2021

import scala.annotation.tailrec
import scala.collection.immutable.TreeSet
import scala.util.Random

object Day23 {
  def main(args: Array[String]): Unit = {
    val p1Start = Map(2 -> "DB", 4 -> "CC", 6 -> "AD", 8 -> "BA")
    val p1Goal  = Map(2 -> "AA", 4 -> "BB", 6 -> "CC", 8 -> "DD")
    println(s"Part I: ${minimumCost(p1Start, p1Goal)}")

    val p2Start = Map(2 -> "DDDB", 4 -> "CCBC", 6 -> "ABAD", 8 -> "BACA")
    val p2Goal  = Map(2 -> "AAAA", 4 -> "BBBB", 6 -> "CCCC", 8 -> "DDDD")
    println(s"Part II: ${minimumCost(p2Start, p2Goal)}")
  }

  private val moveCost   = Map('A' -> 1, 'B' -> 10, 'C' -> 100, 'D' -> 1000)
  private val roomFor    = Map('A' -> 2, 'B' -> 4, 'C' -> 6, 'D' -> 8)
  private val hallwayPos = Seq(0, 1, 3, 5, 7, 9, 10)

  case class Board(rooms: Map[Int, String], hallway: Map[Int, Char])
  case class State(cost: Int, id: Int, board: Board)
  type SearchState = (TreeSet[State], Map[Board, Int])

  def minimumCost(start: Map[Int, String], goal: Map[Int, String]): Int = {
    val rSize = goal.values.head.length

    @tailrec
    def dfs(searchState: SearchState): Int = {
      val (toVisit, visited)         = searchState
      val State(totalCost, _, board) = toVisit.head

      if (board.rooms == goal) totalCost
      else dfs((roomEnters(board, rSize) ++ roomExits(board, rSize))
        .foldLeft(toVisit.tail, visited) { case ((toVisit, visited), (nextState, cost)) =>
          val nextCost = totalCost + cost
          if (visited.get(nextState).exists(_ <= nextCost)) (toVisit, visited)
          else (toVisit + State(nextCost, id = Random.nextInt, nextState), visited.updated(nextState, nextCost))
        })
    }

    val toVisit = TreeSet(State(cost = 0, id = 0, Board(start, Map.empty)))(Ordering.by(s => (s.cost, s.id)))
    val visited = Map(Board(start, Map.empty) -> 0)
    dfs(toVisit, visited)
  }

  private def roomExits(board: Board, rSize: Int): Iterable[(Board, Int)] = for {
    (rPos, amphipods) <- board.rooms
    hPos              <- reachableHallwayPos(board.hallway, rPos, amphipods)
  } yield exitToHallway(board, rPos, hPos, rSize)

  private def reachableHallwayPos(hallway: Map[Int, Char], rPos: Int, amphipods: String): Iterable[Int] =
    amphipods.headOption
      .filterNot(_ => amphipods.forall(roomFor(_) == rPos))
      .map(_ => hallwayPos.filter(pathClear(rPos, _, hallway)))
      .getOrElse(Seq.empty)

  private def exitToHallway(board: Board, rPos: Int, hPos: Int, rSize: Int): (Board, Int) = {
    val Board(rooms, hallway) = board

    val amphipods  = rooms(rPos)
    val newRooms   = rooms.updated(rPos, amphipods.tail)
    val newHallway = hallway.updated(hPos, amphipods.head)
    val cost       = (hPos.max(rPos) - hPos.min(rPos) + rSize - amphipods.length + 1) * moveCost(amphipods.head)

    (Board(newRooms, newHallway), cost)
  }

  private def roomEnters(board: Board, rSize: Int): Iterable[(Board, Int)] =
    board.hallway.collect {
      case (hPos, amphipod) if canEnterRoom(board, hPos, amphipod) =>
        enterRoom(board, hPos, rSize)
    }

  private def canEnterRoom(board: Board, hPos: Int, amphipod: Char) = {
    val hallwayStart = if (hPos > roomFor(amphipod)) hPos - 1 else hPos + 1
    pathClear(hallwayStart, roomFor(amphipod), board.hallway) &&
    board.rooms(roomFor(amphipod)).forall(_ == amphipod)
  }

  private def enterRoom(board: Board, hPos: Int, rSize: Int): (Board, Int) = {
    val Board(rooms, hallway) = board

    val rPos       = roomFor(hallway(hPos))
    val newRooms   = rooms.updated(rPos, hallway(hPos) +: rooms(rPos))
    val newHallway = hallway.removed(hPos)
    val cost       = (hPos.max(rPos) - hPos.min(rPos) + rSize - rooms(rPos).length) * moveCost(hallway(hPos))

    (Board(newRooms, newHallway), cost)
  }

  private def pathClear(rPos: Int, hPos: Int, hallway: Map[Int, Char]): Boolean =
    !(hPos.min(rPos) to hPos.max(rPos)).exists(hallway.contains)
}
