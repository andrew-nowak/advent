import lib.Support

object `1` extends App with Support:
  val elfCalories = load2dIntSeq(newline + newline, newline).map(_.sum).sorted.reverse

  println(elfCalories.head)
  println(elfCalories.take(3).sum)
