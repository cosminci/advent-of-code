package com.github.cosminci.aoc._2020

import com.github.cosminci.aoc.utils

import scala.util.matching.Regex

object Day4 {

  def main(args: Array[String]): Unit = {
    val passports = utils
      .loadInputAsStrings("2020/day4.txt")
      .appended("")
      .foldLeft(Seq.empty[Map[String, String]], Map.empty[String, String]) { case ((complete, inProgress), line) =>
        if (line.isEmpty) (complete :+ inProgress, Map.empty)
        else (complete, inProgress ++ line.trim.split(' ').map(_.split(':')).map(arr => arr.head -> arr.last))
      }

    println(s"Part 1: ${countValidFieldPassports(passports._1)}")
    println(s"Part 2: ${countValidPassports(passports._1)}")
  }

  sealed trait Rule {
    def isValid(s: String): Boolean
  }
  case class Simple(regex: Regex) extends Rule {
    override def isValid(s: String): Boolean = regex.matches(s)
  }
  case class Bounded(regex: Regex, min: Int, max: Int) extends Rule {
    override def isValid(s: String): Boolean = s match {
      case regex(v) => v.toInt >= min && v.toInt <= max
      case _ => false
    }
  }
  case class Or(rule1: Rule, rule2: Rule) extends Rule {
    override def isValid(s: String): Boolean = rule1.isValid(s) || rule2.isValid(s)
  }

  private val requiredFields = Map(
    ("byr", Bounded("^([0-9]{4})$".r, min = 1920, max = 2002)),
    ("iyr", Bounded("^([0-9]{4})$".r, min = 2010, max = 2020)),
    ("eyr", Bounded("^([0-9]{4})$".r, min = 2020, max = 2030)),
    ("hgt", Or(Bounded("^([0-9]+)cm$".r, min = 150, max = 193), Bounded("^([0-9]+)in$".r, min = 59, max = 76))),
    ("hcl", Simple("^#[0-9a-f]{6}$".r)),
    ("ecl", Simple("^(amb|blu|brn|gry|grn|hzl|oth)$".r)),
    ("pid", Simple("^[0-9]{9}$".r))
  )

  def countValidFieldPassports(passports: Seq[Map[String, String]]): Int =
    passports.count(fields => requiredFields.keys.forall(fields.keySet.contains))

  def countValidPassports(passports: Seq[Map[String, String]]): Int =
    passports.count { fields =>
      requiredFields.forall { case (name, rule) =>
        fields.get(name).exists(rule.isValid)
      }
    }
}
