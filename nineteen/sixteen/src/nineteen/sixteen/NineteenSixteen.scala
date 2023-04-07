package nineteen.sixteen

import lib.Support
import scala.annotation.tailrec

object NineteenSixteen extends App with Support {
  val testData =
    """
      |02935109699940807407585447034323
      |""".stripMargin.trim
  val input = load

  @tailrec
  def runFFT(signal: Seq[Int], times: Int): Seq[Int] = {
    if (times == 0) signal
    else runFFT(phase(signal), times - 1)
  }

  @tailrec
  def runp2(signal: Seq[Int], times: Int): Seq[Int] = {
    if (times == 0) signal
    else runp2(signal.scanRight(0)((a, b) => (a + b) % 10).dropRight(1), times - 1)
  }

  def phase(signal: Seq[Int]): Seq[Int] = {
    signal.indices.map { i =>
      val value = for (k <- i until signal.length) yield {
        ((k + 1) / (i + 1)) % 4 match {
          case 1 => signal(k)
          case 3 => -signal(k)
          case _ => 0
        }
      }
      Math.abs(value.sum) % 10
    }
  }

  def run(data: String) = {
    val in = intSeq(data, delimiter = "")

    println(runFFT(in, times = 100).take(8).mkString)

    val reps = 10000
    val inP2 = Seq.fill(reps)(in).flatten
    val skip = in.take(7).mkString.toInt
    val tot = runp2(inP2.drop(skip), 100)
    println(tot.take(8).mkString)
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
}
