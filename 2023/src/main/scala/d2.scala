import lib.Support

object d2 extends App with Support {
  val redR = """(\d+) red""".r
  val greenR = """(\d+) green""".r
  val blueR = """(\d+) blue""".r

  val testData =
    """
      |Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
      |Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
      |Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
      |Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
      |Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
      |""".trim.stripMargin
  val input = load

  def isvalid(show: String): Boolean = {
    (redR.findFirstMatchIn(show), greenR.findFirstMatchIn(show), blueR.findFirstMatchIn(show)) match {
      case (Some(red), _, _) if red.group(1).toInt > 12 => false
      case (_, Some(green), _) if green.group(1).toInt > 13 => false
      case (_, _, Some(blue)) if blue.group(1).toInt > 14 => false
      case _ => true
    }
  }

  def run(data: String): Unit = {
    val in = stringSeq(data)

    lazy val p1 = in.flatMap { line =>
      val gameN = line.drop(5).takeWhile(_.isDigit).toInt
      val shows = line.dropWhile(_ != ':').drop(2).split("; ")
      if (shows.forall(isvalid)) {
        Some(gameN)
      } else None
    }.sum

    lazy val p2 = in.map { line =>
      val shows = line.dropWhile(_ != ':').drop(2).split("; ")
      val (r, g, b) = shows.foldLeft((0, 0, 0)) { case ((ra, ga, ba), show) =>
        val red = math.max(ra, redR.findFirstMatchIn(show).map(_.group(1).toInt).getOrElse(-1))
        val green = math.max(ga, greenR.findFirstMatchIn(show).map(_.group(1).toInt).getOrElse(-1))
        val blue = math.max(ba, blueR.findFirstMatchIn(show).map(_.group(1).toInt).getOrElse(-1))
        (red, green, blue)
      }
      r * g * b
    }.sum

    println(p1)

    println(p2)
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
}
