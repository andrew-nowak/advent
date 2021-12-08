package twentyone.seven

import lib.Support

object TwentyoneSeven extends App with Support {

  def sigma(n: Int) = n * (n + 1) / 2

  val i = loadIntSeq(",")

  val medianPos = i.sorted.apply(i.size / 2)
  val part1 = i.map(pos => (pos - medianPos).abs).sum

  println(part1)

  val alignments = 0 to i.max

  val part2 = alignments
    .map(alignment => {
      val distances = i.map(pos => (pos - alignment).abs)
      val fuelConsumed = distances.map(sigma).sum
      fuelConsumed
    })
    .min

  println(part2)
}
