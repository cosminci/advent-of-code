package com.github.cosminci.aoc._2018

import com.github.cosminci.aoc.utils

import scala.collection.immutable.TreeSet

object Day13 {

  def main(args: Array[String]): Unit = {
    val grid  = utils.loadInputAsStrings("2018/day13.txt")
    val carts = findCarts(grid)

    println(s"Part 1: ${locationOfFirstCrash(carts, cleanGrid(grid, carts))}")
    println(s"Part 2: ${locationOfLastRemainingCart(carts, cleanGrid(grid, carts))}")
  }

  final case class Pos(x: Int, y: Int)
  final case class Cart(pos: Pos, dir: Int, nextTurn: Int)
  implicit val cartOrdering: Ordering[Cart] = Ordering.by(c => (c.pos.y, c.pos.x))

  def locationOfFirstCrash(carts: Seq[Cart], grid: Seq[String]): String = {
    @annotation.tailrec
    def dfs(carts: TreeSet[Cart]): String = {
      val (remaining, collisions) = tickCarts(carts, grid)
      if (collisions.nonEmpty) s"${collisions.head.x},${collisions.head.y}" else dfs(remaining)
    }
    dfs(TreeSet.from(carts))
  }

  def locationOfLastRemainingCart(carts: Seq[Cart], grid: Seq[String]): String = {
    @annotation.tailrec
    def dfs(carts: TreeSet[Cart]): String = {
      val (remaining, _) = tickCarts(carts, grid)
      if (remaining.size == 1) s"${remaining.head.pos.x},${remaining.head.pos.y}" else dfs(remaining)
    }
    dfs(TreeSet.from(carts))
  }

  private def tickCarts(carts: TreeSet[Cart], grid: Seq[String]) = {
    @annotation.tailrec
    def dfs(initial: TreeSet[Cart], moved: TreeSet[Cart], collisions: Seq[Pos]): (TreeSet[Cart], Seq[Pos]) =
      initial.headOption match {
        case None => (moved, collisions)
        case Some(cart) =>
          val remaining = initial.tail
          val movedCart = move(cart, grid)
          if (remaining.contains(movedCart)) dfs(remaining.excl(movedCart), moved, collisions :+ movedCart.pos)
          else if (moved.contains(movedCart)) dfs(remaining, moved.excl(movedCart), collisions :+ movedCart.pos)
          else dfs(remaining, moved.incl(movedCart), collisions)
      }
    dfs(initial = carts, moved = TreeSet.empty, collisions = Seq.empty)
  }

  private val dirs = Seq('^', '<', 'v', '>')

  private def move(cart: Cart, grid: Seq[String]) = {
    val nextX = cart.pos.x + (cart.dir       % 2) * (cart.dir - 2)
    val nextY = cart.pos.y + ((cart.dir + 1) % 2) * (cart.dir - 1)

    val (nextDir, nextTurn) = grid(nextY)(nextX) match {
      case '/'  => (turnCorner(cart.dir, '/'), cart.nextTurn)
      case '\\' => (turnCorner(cart.dir, '\\'), cart.nextTurn)
      case '+'  => (turnJunction(cart.dir, cart.nextTurn), (cart.nextTurn + 1) % 3)
      case _    => (cart.dir, cart.nextTurn)
    }

    Cart(Pos(nextX, nextY), nextDir, nextTurn)
  }

  private def turnJunction(dir: Int, turnIdx: Int) =
    if (turnIdx == 1) dir
    else if (turnIdx == 0) (dir + 1) % 4
    else (dir + 3)                   % 4

  private def turnCorner(dir: Int, track: Char) =
    (dir, track) match {
      case (0, '\\') | (1, '/') | (2, '\\') | (3, '/') => (dir + 1) % dirs.length
      case (0, '/') | (1, '\\') | (2, '/') | (3, '\\') => (dir + 3) % dirs.length
    }

  private def findCarts(grid: Seq[String]) =
    for {
      y <- grid.indices
      x <- grid(y).indices
      if dirs.contains(grid(y)(x))
    } yield Cart(Pos(x, y), dirs.indexOf(grid(y)(x)), 0)

  private def cleanGrid(grid: Seq[String], carts: Seq[Cart]) =
    carts.foldLeft(grid) { case (graph, cart) =>
      graph.updated(cart.pos.y, graph(cart.pos.y).updated(cart.pos.x, if (cart.dir % 2 == 0) '|' else '-'))
    }

}
