import lib.Support

object d6 extends App with Support {

  val testData =
    """
      |Time:      7  15   30
      |Distance:  9  40  200
      |""".trim.stripMargin
  val input = load

  def run(data: String): Unit = {
    val in = stringSeq(data)

    val times = in.head.split(" ").flatMap(_.toLongOption)
    val distances = in.last.split(" ").flatMap(_.toLongOption)

    val races = times zip distances

    def solveRace(time: Long, dist: Long): Long = {
      /* if t' is time spent holding a button, T is race time and t is time spent moving,
       * t = T - t'
       * v = t'
       * d = vt
       * d = t'(T - t')
       * 0 = -t'^2 + Tt' - d
       * 0 = t'^2 - Tt' + d
       * which is a quadratic...
       */
      val t = -time
      val a = (-t + math.sqrt((t * t - 4 * dist).toDouble) / 2.0)
      val b = (-t - math.sqrt((t * t - 4 * dist).toDouble) / 2.0)

      val max = math.floor(
        math.max(a, b) - 0.00000001
      ) // annoying but needed to cater for strict lt/gt
      val min = math.ceil(math.min(a, b) + 0.00000001)
      (max - min + 1).toLong
    }

    lazy val p1 =
      races.map { case (time, dist) => solveRace(time, dist) }.toList.product

    val p2time = in.head.filter(_.isDigit).toLong
    val p2dist = in.last.filter(_.isDigit).toLong
    lazy val p2 = solveRace(p2time, p2dist)

    println(p1)

    println(p2)
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
}
