package codejam.dijkstra

import codejam.Problem
import common._

import scala.annotation.tailrec

object Dijkstra extends Problem {

  override val filenames = List(
    "C-example.in",
    "C-small-practice.in",
    "C-large-practice.in"
  )

  override def process(filename: String) = {
    val linesIterator = Io.readFile(filename, getClass).drop(1) // dropping first line
    val solutions = linesIterator
        .sliding(2, 2) // 2 lines at a time
        .map(lines => (lines(1), lines.head.split(" ")(1).toLong)) // multiply 2nd line by the 2nd part of 1st line
        .toList
        .zipWithIndex
        .map{case ((line, factor), index) => solveLine(line, factor, index + 1)}

    Io.saveToFile(Io.replaceExtension(filename, "out"), solutions)
  }

  def solveLine(line: String, factor: Long, caseIndex: Int): String = {
    val solution = solveCase(new RepeatedArray[Quaternion](line map (Quaternion(_)) toArray, factor)) match {
      case true => "YES"
      case false => "NO"
    }
    println(s"Case #$caseIndex: $solution")
    s"Case #$caseIndex: $solution"
  }

  def solveCase(quaternions: RepeatedArray[Quaternion]): Boolean = {

    val croppedQuaternions = quaternions.withFactor(Math.min(8, quaternions.factor))
    val length = croppedQuaternions.length

    case class Solution(index: Int, reduction: Quaternion, foundFirstMatch: Boolean){
      def move =
        if (index >= length - 1)
          None
        else if (!foundFirstMatch && reduction == I)
          Some(Solution(index + 1, croppedQuaternions(index + 1), foundFirstMatch = true))
        else
          Some(Solution(index + 1, reduction * croppedQuaternions(index + 1), foundFirstMatch))

      val isValid = foundFirstMatch && reduction == J
    }

    @tailrec
    def solveCaseRec(optSolution: Option[Solution]): Boolean = optSolution match {
        case None => false
        case Some(sol) => sol.isValid match {
          case true => true
          case false => solveCaseRec(sol.move)
        }
      }

    if (length < 3 || !reducesTo(quaternions, -ONE))
      false
    else
      solveCaseRec(Some(Solution(0, quaternions(0), foundFirstMatch = false)))

  }

  def reducesTo(quaternions: RepeatedArray[Quaternion], target: Quaternion) =
    quaternions.internal.reduce(_ * _).pow(quaternions.factor) == target

  case class Quaternion(value: Char, sign: Boolean = true){

    def * (that: Quaternion) : Quaternion = {
      val aggregatedSign = this.sign == that.sign
      (this.value, that.value) match {
        case ('1', q2) => Quaternion(q2, aggregatedSign)
        case (q1, q2) if q1 == q2 => -Quaternion('1', aggregatedSign)
        case ('i', '1') => Quaternion('i', aggregatedSign)
        case ('i', 'j') => Quaternion('k', aggregatedSign)
        case ('i', 'k') => -Quaternion('j', aggregatedSign)
        case ('j', '1') => Quaternion('j', aggregatedSign)
        case ('j', 'i') => -Quaternion('k', aggregatedSign)
        case ('j', 'k') => Quaternion('i', aggregatedSign)
        case ('k', '1') => Quaternion('k', aggregatedSign)
        case ('k', 'i') => Quaternion('j', aggregatedSign)
        case ('k', 'j') => -Quaternion('i', aggregatedSign)
      }
    }

    def unary_- : Quaternion = Quaternion(value, !sign)

    def pow(exp: Long) : Quaternion = exp match {
      case 0 => ONE
      case 1 => this
      case n if n % 2 == 0 => (this * this).pow(n / 2)
      case n => this * (this * this).pow((n - 1) / 2)
    }
  }

  val I = Quaternion('i')
  val J = Quaternion('j')
  val K = Quaternion('k')
  val ONE = Quaternion('1')


}


