package com.github.cosminci.aoc._2018

object Day14 {

  def main(args: Array[String]): Unit = {
    val offset = 170641

    println(s"Part 1: ${scoresOfRecipesAfterOffset(offset).mkString}")
    println(s"Part 2: ${recipeCountBeforeScore(offset.toString.map(_.asDigit))}")
  }

  final case class RecipeState(scores: Vector[Int], i: Int, j: Int)

  private def scoresOfRecipesAfterOffset(offset: Int) =
    recipeStateGen.drop(offset + 10).next().scores.slice(offset, offset + 10)

  private val recipeStateGen =
    Iterator.iterate(RecipeState(scores = Vector(3, 7), i = 0, j = 1)) {
      case RecipeState(scores, i, j) =>
        val newScores = combine(scores, i, j)
        RecipeState(newScores, move(i, newScores), move(j, newScores))
    }

  private def recipeCountBeforeScore(scores: Seq[Int]) =
    recipeStateGen.sliding(2)
      .flatMap { case Seq(prev, curr) =>
        val i = curr.scores.indexOfSlice(scores, prev.scores.length - scores.length)
        Option.when(i > 0)(i)
      }.next()

  private def combine(scores: Vector[Int], i: Int, j: Int) =
    scores ++ (scores(i) + scores(j)).toString.map(_.asDigit)

  private def move(i: Int, scores: Seq[Int]) =
    (i + 1 + scores(i)) % scores.size

}
