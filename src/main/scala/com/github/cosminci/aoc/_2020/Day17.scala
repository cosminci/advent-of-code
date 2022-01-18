package com.github.cosminci.aoc._2020

import com.github.cosminci.aoc.utils

object Day17 {

  def main(args: Array[String]): Unit = {
    val grid = utils.loadInputAsStrings("2020/day17.txt")

    println(s"Part 1: ${count3DCubesAfterBootstrap(grid)}")
    println(s"Part 2: ${count4DCubesAfterBootstrap(grid)}")
  }

  sealed trait Cube
  case class Cube3D(x: Int, y: Int, z: Int)         extends Cube
  case class Cube4D(x: Int, y: Int, z: Int, w: Int) extends Cube

  private def count3DCubesAfterBootstrap(grid: Seq[String]): Int = {
    val cubes = initialCubes(grid).map { case (x, y) => Cube3D(x, y, 0) }
    Iterator.iterate(cubes.toSet)(prev => runCycle(prev)).drop(6).next().size
  }

  private def count4DCubesAfterBootstrap(grid: Seq[String]): Int = {
    val cubes = initialCubes(grid).map { case (x, y) => Cube4D(x, y, 0, 0) }
    Iterator.iterate(cubes.toSet)(prev => runCycle(prev)).drop(6).next().size
  }

  private def initialCubes(grid: Seq[String]) =
    for {
      x <- grid.indices
      y <- grid.head.indices
      if grid(x)(y) == '#'
    } yield (x, y)

  private def runCycle[T <: Cube](prevCubes: Set[T]): Set[T] =
    prevCubes
      .map(neighbours)
      .foldLeft((Set.empty[T], Set.empty[T])) { case ((cubes, visited), toExplore) =>
        val newCubes = toExplore.diff(visited).flatMap { cube =>
          val neighbourCount = (neighbours(cube) - cube).count(prevCubes.contains)
          if (prevCubes.contains(cube))
            Option.when(neighbourCount == 2 || neighbourCount == 3)(cube)
          else
            Option.when(neighbourCount == 3)(cube)
        }
        (cubes ++ newCubes, visited ++ toExplore)
      }
      ._1

  private def neighbours[T <: Cube](cube: T): Set[T] =
    cube match {
      case p: Cube3D =>
        neighbours3D(p).asInstanceOf[Set[T]]
      case p: Cube4D =>
        neighbours4D(p).asInstanceOf[Set[T]]
    }

  private def neighbours3D(cube: Cube3D): Set[Cube3D] = {
    val samePlane = Set(
      cube,
      cube.copy(x = cube.x - 1),
      cube.copy(x = cube.x + 1),
      cube.copy(y = cube.y - 1),
      cube.copy(y = cube.y + 1),
      cube.copy(x = cube.x - 1, y = cube.y - 1),
      cube.copy(x = cube.x - 1, y = cube.y + 1),
      cube.copy(x = cube.x + 1, y = cube.y - 1),
      cube.copy(x = cube.x + 1, y = cube.y + 1)
    )
    val upperPlane = samePlane.map(p => p.copy(z = p.z + 1))
    val lowerPlane = samePlane.map(p => p.copy(z = p.z - 1))
    samePlane ++ upperPlane ++ lowerPlane
  }

  private def neighbours4D(cube: Cube4D): Set[Cube4D] =
    neighbours3D(Cube3D(cube.x, cube.y, cube.z)).flatMap { point3d =>
      Set(cube.w - 1, cube.w, cube.w + 1).map(w => Cube4D(point3d.x, point3d.y, point3d.z, w))
    }
}
