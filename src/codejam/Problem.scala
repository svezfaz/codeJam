package codejam


abstract class Problem {

  def process(filename: String): Unit

  val filenames: List[String]

  def main(args: Array[String]) {
    filenames.foreach(process)
  }

}
