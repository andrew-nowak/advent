package nineteen.seven

import lib.{AsyncIntcode, IntcodeBuffer, Support}
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object NineteenSeven extends App with Support {
  val testData =
    """
      |3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0
      |""".stripMargin.trim
  val input = load

  def run(data: String) = {
    val in = longSeq(data, ",")

    def runIntcode(seq: Seq[Long]): Long = {
      import scala.concurrent.ExecutionContext.Implicits.global
      val buffers = seq.map { n =>
        val x = new IntcodeBuffer
        x.give(n)
        x
      } :+ new IntcodeBuffer
      buffers.head.give(0)

      val runners = Future.sequence(
        buffers.sliding(2).map { case Seq(a, b) => AsyncIntcode.buildAndRunWithBuffers(in, a, b) }.toList
      )

      val res = Await.result(runners.flatMap(_.last.asInstanceOf[IntcodeBuffer].take), 2.seconds)
      res
    }

    def runLooped(seq: Seq[Long]): Long = {
      import scala.concurrent.ExecutionContext.Implicits.global
      val buffers = seq.map { n =>
        val x = new IntcodeBuffer
        x.give(n)
        x
      }
      buffers.head.give(0)
      val intcodes = (buffers :+ buffers.head)
        .sliding(2)
        .map { case Seq(a, b) => AsyncIntcode.buildAndRunWithBuffers(in, a, b) }
        .toList

      Await.result(Future.sequence(intcodes).flatMap(_.last.asInstanceOf[IntcodeBuffer].take), 2.seconds)
    }

    val p1 = (0L to 4L).permutations.map(runIntcode).max
    println(p1)

    val p2 = (5L to 9L).permutations.map(runLooped).max
    println(p2)
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
}
