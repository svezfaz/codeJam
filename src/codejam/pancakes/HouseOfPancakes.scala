package codejam.pancakes

import codejam.Problem
import common.Io

object HouseOfPancakes extends Problem {

  override val filenames = List(
    "B-example.in",
    "B-small-practice.in",
    "B-large-practice.in")

  override def process(filename: String) = {
        val linesIterator = Io.readFile(filename, getClass).drop(2) //don't need first line
        val solutions = linesIterator
          .sliding(2, 2) //taking 2 blocks of 2 lines at a time
          .map(_(1)) //getting only the second line of each block
          .map(_.split(" ").map(_.toInt).toList) //getting list of Int from each line
          .toList
          .zipWithIndex
          .map((lineAndIndex:(List[Int], Int)) => solveLine(lineAndIndex._1, lineAndIndex._2 + 1))

        Io.saveToFile(Io.replaceExtension(filename, "out"), solutions)
  }

  def solveLine(pancakes: List[Int], caseIndex: Int): String = {
    val solution = solveCase(Pancakes(pancakes))
    println(s"Case #$caseIndex: $solution")
    s"Case #$caseIndex: $solution"
  }

  case class Pancakes(pancakes: List[Int]){
    val max = pancakes.max
  }

  def solveCase(pancakes: Pancakes): Int = {
    val solutions = (1 to pancakes.max).map(solutionGivenRegularMinutes(pancakes, _)).sorted.head
    pancakes.max.min(solutions)
  }

  def solutionGivenRegularMinutes(pancakes: Pancakes, regularMinutes: Int): Int =
    pancakes.pancakes.map(pancakePlate => (pancakePlate - 1) / regularMinutes).sum + regularMinutes
}
