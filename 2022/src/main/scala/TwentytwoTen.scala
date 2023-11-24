package twentytwo.ten

import lib.Support

object TwentytwoTen extends App with Support {
  val testData =
    """
       |""".stripMargin.trim
  val input = load

  def run(data: String) = {
    val in = stringSeq(data)

    val p1 = in.foldLeft[List[Int]](List(1))((registerHistory, instr) => {
      instr.split(" ") match {
        case Array("noop")    => registerHistory :+ registerHistory.last
        case Array("addx", n) => registerHistory ++ List(registerHistory.last, registerHistory.last + n.toInt)
      }
    })
    if (p1.length > 220) {
      def sig(i: Int) = p1(i - 1) * i
      val res = List(sig(20), sig(60), sig(100), sig(140), sig(180), sig(220)).sum
      println(res)
    }

    // p2
    p1.grouped(40)
      .map(_.zipWithIndex.map {
        case (spritePos, px) if Math.abs(px - spritePos) <= 1 => '#'
        case _                                                => ' '
      })
      .map(_.mkString)
      .foreach(println)
  }

  println("--- testdata ---")
  // run(testData)
  println("--- real ---")
  run(input)
}
