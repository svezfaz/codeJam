package codejam.standingovation

import codejam.Problem
import common._

object StandingOvation extends Problem {

  override val filenames = List(
    "A-example.in",
    "A-small-practice.in",
    "A-large-practice.in")

  override def process(filename: String) = {
    val linesIterator = Io.readFile(filename, getClass)
    val solutions = linesIterator.drop(1).toList.zipWithIndex
      .map((lineAndIndex:(String, Int)) => solveLine(lineAndIndex._1, lineAndIndex._2 + 1))

    Io.saveToFile(Io.replaceExtension(filename, "out"), solutions)
  }

  def solveLine(line: String, caseIndex: Int): String = {
    val people = line.split(" ")(1).toList.map(_.asDigit).zipWithIndex
    val solution = solveCase(people)
    s"Case #$caseIndex: $solution"
  }

  def solveCase(people: List[(Int, Int)]): Int = {
    people.foldLeft((0,0))((neededAndApplauding: (Int, Int), howManyPeopleAndShyness: (Int, Int)) =>
      shynessStep(howManyPeopleAndShyness._1, howManyPeopleAndShyness._2, neededAndApplauding._1, neededAndApplauding._2))
      ._1
  }

  def shynessStep(howManyPeople: Int, shyness: Int, neededPersons: Int, applaudingPersons: Int): (Int, Int) = {
    val additionalNeededPersons = math.max(shyness - applaudingPersons, 0)
    (additionalNeededPersons + neededPersons, applaudingPersons + additionalNeededPersons + howManyPeople)
  }

}


