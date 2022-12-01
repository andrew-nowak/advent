package twentytwo.one

import lib.Support

object TwentytwoOne extends App with Support {
  val i = loadStringSeq

  val rankedElves: List[Int] = i.foldLeft((List.empty[Int], 0))((acc, cals) => {
    if (cals.isEmpty) {
      (acc._1 :+ acc._2, 0)
    } else {
      (acc._1, acc._2 + cals.toInt)
    }
  })._1

  println(rankedElves.max)
  println(rankedElves.sorted.takeRight(3).sum)
}
