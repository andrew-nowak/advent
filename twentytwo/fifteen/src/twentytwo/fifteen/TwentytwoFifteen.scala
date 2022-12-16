package twentytwo.fifteen

import lib.{Coord, Support}

case class Sensor(loc: Coord, radius: Int)

object TwentytwoFifteen extends App with Support {
  val reg = """Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)""".r
  val testData =
    """
      |Sensor at x=2, y=18: closest beacon is at x=-2, y=15
      |Sensor at x=9, y=16: closest beacon is at x=10, y=16
      |Sensor at x=13, y=2: closest beacon is at x=15, y=3
      |Sensor at x=12, y=14: closest beacon is at x=10, y=16
      |Sensor at x=10, y=20: closest beacon is at x=10, y=16
      |Sensor at x=14, y=17: closest beacon is at x=10, y=16
      |Sensor at x=8, y=7: closest beacon is at x=2, y=10
      |Sensor at x=3, y=10: closest beacon is at x=2, y=10
      |Sensor at x=2, y=0: closest beacon is at x=2, y=10
      |Sensor at x=0, y=11: closest beacon is at x=2, y=10
      |Sensor at x=20, y=14: closest beacon is at x=25, y=17
      |Sensor at x=17, y=20: closest beacon is at x=21, y=22
      |Sensor at x=16, y=7: closest beacon is at x=15, y=3
      |Sensor at x=14, y=3: closest beacon is at x=15, y=3
      |Sensor at x=20, y=1: closest beacon is at x=15, y=3
      |""".stripMargin.trim
  val input = load

  def ordered(a: Int, b: Int, c: Int, d: Int): Boolean = a <= b && b <= c && c <= d

  def run(data: String) = {
    val in = stringSeq(data).map { case reg(sensorX, sensorY, beaconX, beaconY) =>
      val loc = Coord(sensorX.toInt, sensorY.toInt)
      val radius = loc.manhattan(Coord(beaconX.toInt, beaconY.toInt))
      Sensor(loc, radius)
    }
    val row = if (in.size < 20) 10 else 2000000 // example data?
    val height = if (in.size < 20) 20 else 4000000

    val ranges = in.flatMap { case Sensor(Coord(x, y), radius) =>
      val width = radius - Math.abs(y - row)
      if (width >= 0) Some((x - width) -> (x + width)) else None
    }

    val cvg = ranges
      .sortBy(r => r._2 - r._1)
      .reverse
      .foldLeft(List.empty[(Int, Int)])((acc, range) => {
        val reducedRange = acc.foldLeft(range)((thisRange, seenRange) => {
          if (ordered(thisRange._1, seenRange._1, thisRange._2, seenRange._2)) {
            (thisRange._1, seenRange._1)
          } else if (ordered(seenRange._1, thisRange._1, seenRange._2, thisRange._2)) {
            (seenRange._2, thisRange._2)
          } else if (ordered(seenRange._1, thisRange._1, thisRange._2, seenRange._2)) {
            (0, -1)
          } else if (ordered(thisRange._1, seenRange._1, seenRange._2, thisRange._2)) {
            throw new Error("case removed by prior sorting")
          } else {
            thisRange
          }
        })
        if (reducedRange == (0, -1)) acc else acc :+ reducedRange
      })
      .map(r => r._2 - r._1)
      .sum

    // - -                     1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2
    // 2 1 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5
    //                             x
    //         x-------------------#---x
    //         #-#-#
    //         #
    // x-------#
    //                                     x---------------x
    //                                 #---#-#-#
    println(ranges)
    val p1 = cvg // - overlap.sum
    println(p1)

    val p2 = (0 to height)
      .collectFirst(
        (
            (n: Int) => {
              val ranges = in.flatMap { case Sensor(Coord(x, y), radius) =>
                val width = radius - Math.abs(y - n)
                if (width >= 0) Some((x - width) -> (x + width)) else None
              }

              val cvg = ranges
                .sortBy(r => r._2 - r._1)
                .reverse
                .foldLeft(List.empty[(Int, Int)])((acc, range) => {
                  val reducedRange = acc.foldLeft(range)((thisRange, seenRange) => {
                    if (ordered(thisRange._1, seenRange._1, thisRange._2, seenRange._2)) {
                      (thisRange._1, seenRange._1)
                    } else if (ordered(seenRange._1, thisRange._1, seenRange._2, thisRange._2)) {
                      (seenRange._2, thisRange._2)
                    } else if (ordered(seenRange._1, thisRange._1, thisRange._2, seenRange._2)) {
                      (0, -1)
                    } else if (ordered(thisRange._1, seenRange._1, seenRange._2, thisRange._2)) {
                      throw new Error("case removed by prior sorting")
                    } else {
                      thisRange
                    }
                  })
                  if (reducedRange == (0, -1)) acc else acc :+ reducedRange
                })

              cvg
                .sortWith {
                  case ((a, _), (c, _)) if a < c => true
                  case ((a, _), (c, _)) if a > c => false
                  case ((_, b), (_, d))          => b < d

                }
                .sliding(2)
                .collectFirst { case Seq((_, amax), (bmin, _)) if amax != bmin => bmin - 1 }
                .map(x => (x, n))
            }
        ).unlift
      )

    val res = p2.get._1.toLong * 4000000L + p2.get._2.toLong
    println(res)
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
}
