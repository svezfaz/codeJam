package common

class RepeatedArray[A](val internal: Array[A], val factor: Long) {

  require(factor > 0, "factor must be a strictly positive integer")

  val length = factor * internal.length

  def apply(index: Long): A =
    if (index >= length || index < 0)
      throw new ArrayIndexOutOfBoundsException(index.toString)
    else
      internal((index % internal.length).toInt)

  def withFactor(newFactor: Long) = new RepeatedArray[A](internal, newFactor)
}
