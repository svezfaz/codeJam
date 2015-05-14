package codejam.dijkstra

import codejam.Problem
import common._

import scala.annotation.tailrec

object Dijkstra extends Problem {

  override val filenames = List(
    "C-example.in")//,
    //"C-small-practice.in",
    //"C-large-practice.in")

  override def process(filename: String) = {
    val linesIterator = Io.readFile(filename, getClass).drop(1) // dropping first line
    val solutions = linesIterator
      .sliding(2, 2) // 2 lines at a time
      .map(lines => lines(1) * lines.head.split(" ")(1).toInt) // multiply 2nd line by the 2nd digit of 1st line
      .toList
      .zipWithIndex
      .map((lineAndIndex:(String, Int)) => solveLine(lineAndIndex._1, lineAndIndex._2 + 1))

    Io.saveToFile(Io.replaceExtension(filename, "out"), solutions)
  }

  def solveLine(line: String, caseIndex: Int): String = {
    val solution = solveCase(toQuaternions(line)) match {
      case true => "YES"
      case false => "NO"
    }
    s"Case #$caseIndex: $solution"
  }

  def solveCase(quaternions: List[Quaternion]): Boolean = {
    solveCaseRec(quaternions, 1, 2)
  }

  @tailrec
  def solveCaseRec(quaternions: List[Quaternion], index1: Int, index2:Int): Boolean = {
    lazy val length = quaternions.length
    
    def moveIndex1: (Int, Int) = ???
      //todo move index1 and possibly index2. check for limit cases

    def moveIndex2: (Int, Int) = ???
      //todo move index2. check for limit cases

    if (reducesTo(quaternions.take(index1), I)){
      if (reducesTo(quaternions.drop(index1).take(index2 - index1), J)){
        if (reducesTo(quaternions.drop(index2), K)){
          true
        } else {
          solveCaseRec(quaternions, moveIndex1._1, moveIndex1._2)
        }
      } else{
        solveCaseRec(quaternions, moveIndex2._1, moveIndex2._2)
      }
    } else {
      solveCaseRec(quaternions, moveIndex1._1, moveIndex1._2)
    }
  }

  def toQuaternions(s: String): List[Quaternion] = s map toQuaternion toList

  def toQuaternion(c: Char): Quaternion = c match {
    case 'j' => Quaternion(JVal)
    case 'i' => Quaternion(IVal)
    case 'k' => Quaternion(KVal)
    case x => throw new IllegalArgumentException(x + " is not a quaternion value")
  }

  def reducesTo(quaternions: List[Quaternion], target: Quaternion) = quaternions.reduce(_ * _) == target

  def step(quaternions: List[Quaternion], target: Quaternion) = {
    val temp = (1 to quaternions.length).map(index => (index, quaternions.take(index)))
      .filter(pair => reducesTo(pair._2, target)).map(_._1).map(quaternions.drop).toList
    println(temp)
    temp
  }

  def isSolution(quaternions: List[Quaternion], index1: Int, index2:Int): Boolean = {
    reducesTo(quaternions.take(index1), I) &&
      reducesTo(quaternions.drop(index1).take(index2 - index1), J) &&
      reducesTo(quaternions.drop(index2), K)
  }


  sealed trait QuaternionVal
  case object IVal extends QuaternionVal
  case object JVal extends QuaternionVal
  case object KVal extends QuaternionVal
  case object ONEVal extends QuaternionVal

  case class Quaternion(value: QuaternionVal, sign: Boolean = true){
    def * (that: Quaternion) : Quaternion = {
      val aggregatedSign = this.sign == that.sign
      (this.value, that.value) match {
        case (ONEVal, q2) => Quaternion(q2, aggregatedSign)
        case (q1, q2) if q1 == q2 => -Quaternion(ONEVal, aggregatedSign)
        case (IVal, ONEVal) => Quaternion(IVal, aggregatedSign)
        case (IVal, JVal) => Quaternion(KVal, aggregatedSign)
        case (IVal, KVal) => -Quaternion(JVal, aggregatedSign)
        case (JVal, ONEVal) => Quaternion(JVal, aggregatedSign)
        case (JVal, IVal) => -Quaternion(KVal, aggregatedSign)
        case (JVal, KVal) => Quaternion(IVal, aggregatedSign)
        case (KVal, ONEVal) => Quaternion(KVal, aggregatedSign)
        case (KVal, IVal) => Quaternion(JVal, aggregatedSign)
        case (KVal, JVal) => -Quaternion(IVal, aggregatedSign)
      }
    }

    def unary_- : Quaternion = Quaternion(value, !sign)
  }

  val I = Quaternion(IVal)
  val J = Quaternion(JVal)
  val K = Quaternion(KVal)


}


