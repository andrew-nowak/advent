import lib.Support

import scala.annotation.tailrec

object d9 extends App with Support {
  val testData =
    """
      |0 3 6 9 12 15
      |1 3 6 10 15 21
      |10 13 16 21 30 45
      |""".trim.stripMargin
  val input = load

  private def predict(values: Seq[Long]): Long = predict(values, 0L)

  @tailrec private def predict(values: Seq[Long], tot: Long): Long = {
    if (values.forall(_ == 0L)) values.last + tot
    else
      predict(
        values.zip(values.tail).map { case (a, b) => b - a },
        tot + values.last
      )
  }

  private def extrapolate(values: Seq[Long]): Long = {
    if (values.forall(_ == 0L)) 0L
    else
      values.head - extrapolate(
        values.zip(values.tail).map { case (a, b) => b - a }
      )
  }

  def run(data: String): Unit = {
    val start = System.nanoTime()

    val in = `2dLongSeq`(data)

    lazy val p1 = in.map(predict).sum

    lazy val p2 = in.map(extrapolate).sum

    println(p1)

    println(p2)

    val end = System.nanoTime()
    println(s"Done in ${(end - start).toDouble / 1_000_000} ms")
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
}
