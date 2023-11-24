package twentytwo.three

import lib.Support

object TwentytwoThree extends App with Support {
  val i = loadStringSeq

  val prioritise: Char => Int = {
    case c if c >= 'a' && c <= 'z' => c - 'a' + 1
    case c                         => c - 'A' + 27
  }

  val p1 = i
    .map(sack => {
      val (left, right) = sack.splitAt(sack.length / 2)
      (left.toSet intersect right.toSet).head
    })
    .map(prioritise)
    .sum

  val p2 = i
    .grouped(3)
    .map(sacks => sacks.map(_.toSet).reduce(_ intersect _).head)
    .map(prioritise)
    .sum

  println(p1)
  println(p2)
}
