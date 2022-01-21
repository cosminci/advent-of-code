package com.github.cosminci.aoc._2020

import com.github.cosminci.aoc.utils

object Day21 {

  def main(args: Array[String]): Unit = {
    val input   = utils.loadInputAsStrings("2020/day21.txt")
    val recipes = parseInput(input)

    println(s"Part 1: ${countSafeIngredientOccurrences(recipes)}")
    println(s"Part 2: ${dangerousIngredients(recipes)}")
  }

  type Ingredient = String
  type Allergen   = String

  def countSafeIngredientOccurrences(foods: Seq[(Set[Ingredient], Set[Allergen])]): Int = {
    val allergenicIngredients = allergenMap(foods).values.flatten.toSet
    foods
      .flatMap { case (ingredients, _) => ingredients.toSeq }
      .count { ingredient => !allergenicIngredients.contains(ingredient) }
  }

  def dangerousIngredients(foods: Seq[(Set[Ingredient], Set[Allergen])]): String = {
    assignIngredients(assigned = Map.empty, unassigned = allergenMap(foods).toSeq).head.toSeq
      .sortBy { case (_, allergen) => allergen }
      .map { case (ingredient, _) => ingredient }
      .mkString(",")
  }

  private def assignIngredients(
      assigned: Map[Ingredient, Allergen],
      unassigned: Seq[(Allergen, Set[Ingredient])]
  ): Seq[Map[Ingredient, Allergen]] =
    unassigned match {
      case Seq() => Seq(assigned)
      case (allergen, ingredients) +: tail =>
        ingredients.toSeq.filterNot(assigned.contains).flatMap { ingredient =>
          assignIngredients(assigned + (ingredient -> allergen), tail)
        }
    }

  private def allergenMap(foods: Seq[(Set[Ingredient], Set[Allergen])]): Map[Allergen, Set[Ingredient]] =
    foods.foldLeft(Map.empty[Allergen, Set[Ingredient]]) { case (allergenMap, (ingredients, allergens)) =>
      allergens.foldLeft(allergenMap) { case (allergenMap, allergen) =>
        allergenMap.get(allergen) match {
          case None                  => allergenMap.updated(allergen, ingredients)
          case Some(prevIngredients) => allergenMap.updated(allergen, ingredients.intersect(prevIngredients))
        }
      }
    }

  private def parseInput(input: Seq[String]) =
    input.map { line =>
      val Seq(ingredientsStr, allergensStr) = line.dropRight(1).split("\\(contains ").toSeq
      val ingredients                       = ingredientsStr.trim().split(" ").toSet
      val allergens                         = allergensStr.split(", ").toSet
      (ingredients, allergens)
    }
}
