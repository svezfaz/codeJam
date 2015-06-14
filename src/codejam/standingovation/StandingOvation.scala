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
      .map{case (line, index) => solveLine(line, index + 1)}

    Io.saveToFile(Io.replaceExtension(filename, "out"), solutions)
  }

  def solveLine(line: String, caseIndex: Int): String = {
    val people = line.split(" ")(1).toList.map(_.asDigit).zipWithIndex
    val solution = solveCase(people)
    s"Case #$caseIndex: $solution"
  }

  def solveCase(people: List[(Int, Int)]): Int = {
    people.foldLeft((0,0)){
      case ((needed, applauding), (howMany, shyness)) => shynessStep(howMany, shyness, needed, applauding)
    }._1
  }

  def shynessStep(howMany: Int, shyness: Int, needed: Int, applauding: Int): (Int, Int) = {
    val additionalNeeded = math.max(shyness - applauding, 0)
    (additionalNeeded + needed, applauding + additionalNeeded + howMany)
  }

}


