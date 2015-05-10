package common

import scala.io.Source

object Io {

  def readFile(filename: String, clazz: Class[_]): Iterator[String] =
    Source.fromInputStream(clazz.getResourceAsStream(filename)).getLines()

  def saveToFile(filename: String, lines: List[String]): Unit = {
    val printWriter = new java.io.PrintWriter(filename)
    try {
      lines.foreach(printWriter.println)
    }
    finally {
      printWriter.close()
    }
  }

  def replaceExtension(filename: String, newExtension: String): String =
    filename.reverse.dropWhile(_ != '.').reverse + newExtension

}
