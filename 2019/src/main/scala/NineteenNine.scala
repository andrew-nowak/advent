package nineteen.nine

import lib.{AsyncIntcode, Support}

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt

object NineteenNine extends App with Support {
  val testData =
    """
      |1102,34915192,34915192,7,4,7,99,0
      |""".stripMargin.trim
  val input = load

  def run(data: String, input: List[Long]) = {
    val in = longSeq(data, ",")

    val p1 = Await.result(AsyncIntcode.buildAndRunWithIo(in, input), 5.seconds)
    println(p1)

  }

  println("--- testdata ---")
  run(testData, List.empty)
  println("--- real ---")
  run(input, List(1L))
  run(input, List(2L))
}
