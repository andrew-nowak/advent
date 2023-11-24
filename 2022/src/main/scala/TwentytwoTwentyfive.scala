package twentytwo.twentyfive

import lib.Support

object TwentytwoTwentyfive extends App with Support {
  val testData =
    """
      |1=-0-2
      |12111
      |2=0=
      |21
      |2=01
      |111
      |20012
      |112
      |1=-1=
      |1-12
      |12
      |1=
      |122
      |""".stripMargin.trim
  val input = load

  def run(data: String) = {
    val in = stringSeq(data)

    val maxwidth = in.map(_.length).max

    val padded = in.map(_.reverse.padTo(maxwidth, '0').map {
      case '=' => -2
      case '-' => -1
      case n   => Character.getNumericValue(n)
    })

    val summed = padded.reduce((l, r) => l.zip(r).map(pair => pair._1 + pair._2))

    val normalised = summed.foldLeft((Seq.empty[Int], 0))((acc, n) => {
      val rem = (n + acc._2) % 5
      val pows5 = (n + acc._2) / 5
      rem match {
        case 4         => (acc._1 :+ -1, pows5 + 1)
        case 3         => (acc._1 :+ -2, pows5 + 1)
        case 0 | 1 | 2 => (acc._1 :+ rem, pows5)
      }
    })

    val snafud = (normalised._1 :+ normalised._2)
      .map {
        case -2 => "="
        case -1 => "-"
        case n  => n.toString()
      }
      .reverse
      .mkString

    val p1 = snafud.dropWhile(_ == '0')
    println(p1)

  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
}
