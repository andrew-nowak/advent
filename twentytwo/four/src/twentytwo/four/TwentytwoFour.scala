package twentytwo.four

import lib.Support

object TwentytwoFour extends App with Support {
  val testData =
    """
      |2-4,6-8
      |2-3,4-5
      |5-7,7-9
      |2-8,3-7
      |6-6,4-6
      |2-6,4-8
      |""".stripMargin.trim
  val input = load

  def run(data: String) = {
    val in = stringSeq(data)

    val x = in.map(pair => {
      val pairRanges = pair
        .split(",")
        .map(_.split("-").map(_.toInt))
        .map(range => range.head.to(range.last).toSet)
      pairRanges.head -> pairRanges.last
    })
    val p1 = x.count { case (rangea, rangeb) =>
      val n = rangea.intersect(rangeb)
      n == rangea || n == rangeb
    }

    println(p1)

    val p2 = x.count { case (rangea, rangeb) => rangea.intersect(rangeb).nonEmpty }
    println(p2)
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
}
