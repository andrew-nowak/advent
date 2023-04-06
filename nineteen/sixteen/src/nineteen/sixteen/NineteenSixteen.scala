package nineteen.sixteen

import lib.Support
import scala.annotation.tailrec

object NineteenSixteen extends App with Support {
  val testData =
    """
      |02935109699940807407585447034323
      |""".stripMargin.trim
  val input = load

  @tailrec def runFFT(signal: Array[Int], times: Int): Array[Int] = {
    if (times == 0) signal
    else runFFT(NineteenSixteenImpl.phase(signal), times - 1)
  }

  @tailrec def runp2(signal: List[Int], times: Int): List[Int] = {
    if (times == 0) signal
    else runp2(signal.scanRight(0)((a, b) => (a + b) % 10).dropRight(1), times - 1)
  }

  def run(data: String) = {
    val in = intSeq(data, delimiter = "")

    println(runFFT(in.toArray, times = 100).take(8).mkString)

    val reps = 10000
    val inP2 = List.fill(reps)(in).flatten
    val skip = in.take(7).mkString.toInt
    val tot = runp2(inP2.drop(skip), 100)
    println(tot.take(8).mkString)
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
}
