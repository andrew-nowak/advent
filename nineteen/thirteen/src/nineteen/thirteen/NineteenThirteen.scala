package nineteen.thirteen

import lib._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object NineteenThirteen extends App with Support {

  val input = load

  def run(data: String) = {
    val in = longSeq(data, ",")

    val p1 = Await.result(AsyncIntcode.buildAndRunWithIo(in, Nil), 5.seconds)
    println(p1.grouped(3).count(_(2) == 2))

    var paddle = -1
    var ball = -1

    val kbInput = new Provider {
      // to play yourself, uncomment this first implementation of take!
      // val scanner = new Scanner(System.in)

      // override def take: Future[Long] = Future { scanner.next() match {
      //   case "a" => -1
      //   case "d" => 1
      //   case _ => 0
      // }}
      override def take: Future[Long] = Future.successful(ball - paddle)
    }
    var score = 0
    val screenOutput = new Receiver {
      var stack = Seq.empty[Int]
      var screen = Map.empty[Coord, Int]
      override def give(out: Long): Unit = {
        stack = stack :+ out.toInt
        if (stack.size == 3) {
          val Seq(x, y, tile) = stack
          stack = Seq.empty
          if (x == -1 && y == 0) {
            score = tile
          } else {
            screen = screen + (Coord(x, y) -> tile)
            if (tile == 3) paddle = x
            if (tile == 4) ball = x
            // println("-------------------------------")
            // println(s"score: $score")
            // printCoords(
            //   screen, {
            //     case 1 => '█'
            //     case 2 => '#'
            //     case 3 => '='
            //     case 4 => 'o'
            //   }
            // )
          }

        }
      }
    }
    val p2 = AsyncIntcode.buildAndRunWithBuffers(in.updated(0, 2), kbInput, screenOutput)
    Await.result(p2, 600.seconds)
    println(score)
  }

  println("--- real ---")
  run(input)
}
