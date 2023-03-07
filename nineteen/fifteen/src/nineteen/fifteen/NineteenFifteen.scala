package nineteen.fifteen

import lib.Support
import lib.Coord

case class State(area: Map[Coord, Boolean], lastMove: Coord)

object NineteenFifteen extends App with Support {
  val input = load

  def continue(last: Long, state: State): (Long, State) = {
    ???

  }

  def run(data: String) = {
    val in = longSeq(data, ",")

    val p1 = in.head
    println(p1)

    // val p2 = in.last
    // println(p2)
  }

  println("--- real ---")
  run(input)
}
