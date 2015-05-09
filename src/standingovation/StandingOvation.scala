package standingovation

import scala.io.Source

object StandingOvation {
  def main(args: Array[String]) {
    val filenames = List(
      "A-example.in",
      "A-small-practice.in",
      "A-large-practice.in")

    filenames.foreach(process)
  }

  def process(filename: String) = {
    val linesIterator = Source.fromInputStream(getClass.getResourceAsStream(filename)).getLines()
    linesIterator.next()
    val solutionsIterator = linesIterator.drop(0).map(solveLine)

    var caseIndex: Int = 1
    val printWriter = new java.io.PrintWriter(filename.reverse.dropWhile(_ != '.').reverse + "out")
    try {
      for (solution <- solutionsIterator) {
        printWriter.println(solution(caseIndex))
        caseIndex = caseIndex + 1
      }
    }
    finally {
      printWriter.close()
    }
  }

  def shynessStep(howManyPeople: Int, shyness: Int, neededPersons: Int, applaudingPersons: Int): (Int, Int) = {
    val additionalNeededPersons = math.max(shyness - applaudingPersons, 0)
    (additionalNeededPersons + neededPersons, applaudingPersons + additionalNeededPersons + howManyPeople)
  }

  def solve(people: List[(Int, Int)]): Int = {
    people.foldLeft((0,0))((neededAndApplauding: (Int, Int), howManyPeopleAndShyness: (Int, Int)) =>
      shynessStep(howManyPeopleAndShyness._1, howManyPeopleAndShyness._2, neededAndApplauding._1, neededAndApplauding._2))
    ._1
  }

  def solveLine(line: String): (Int => String) = {
    val people = line.split(" ")(1).toList.map(_.asDigit).zipWithIndex
    (caseIndex: Int) => "Case #" + caseIndex + ": " + solve(people)
  }

}


