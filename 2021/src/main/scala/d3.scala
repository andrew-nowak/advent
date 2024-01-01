import lib.Support

import scala.annotation.tailrec

object d3 extends App with Support {
  val i = loadStringSeq

  val allMostCommon = i.transpose
    .map(bits => bits.sorted.apply(bits.length / 2))

  val allLeastCommon = allMostCommon.map {
    case '0' => '1'
    case '1' => '0'
  }

  val gamma = Integer.parseInt(allMostCommon.mkString, 2)
  val epsilon = Integer.parseInt(allLeastCommon.mkString, 2)

  val part1 = gamma * epsilon

  println(part1)

  @tailrec def findRating(diagnostics: Seq[(String, String)], findLeastCommon: Boolean = false): Int = {
    if (diagnostics.length <= 1) Integer.parseInt(diagnostics.head._2, 2)
    else {
      val haystack = diagnostics.map(_._1.head).sorted

      val mostCommon = if (haystack.size % 2 == 0) {
        val lower = haystack(haystack.size / 2 - 1)
        val upper = haystack(haystack.size / 2)
        if (upper == lower) lower else '1'
      } else {
        haystack(haystack.size / 2)
      }

      val target = mostCommon match {
        case '0' if findLeastCommon => '1'
        case '1' if findLeastCommon => '0'
        case otherwise              => otherwise
      }

      findRating(diagnostics.filter(_._1.head == target).map(t => (t._1.tail, t._2)), findLeastCommon)
    }
  }

  val oxygen = findRating(i.map(s => (s, s)))
  val co2 = findRating(i.map(s => (s, s)), findLeastCommon = true)

  val part2 = oxygen * co2

  println(part2)

}
