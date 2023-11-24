package nineteen.eight

import lib.Support

object NineteenEight extends App with Support {
  val testData =
    """
      |0222112222120000
      |""".stripMargin.trim
  val input = load

  def run(data: String, dims: (Int, Int)) = {
    val in = intSeq(data, delimiter = "")

    val min0Layer = in.grouped(dims._1 * dims._2).toSeq.minBy(_.count(_ == 0))

    val p1 = min0Layer.count(_ == 1) * min0Layer.count(_ == 2)
    println(p1)

    val img = in.grouped(dims._1 * dims._2).reduce[Seq[Int]] { case (layera, layerb) =>
      layera.zip(layerb).map {
        case (2, pxb) => pxb
        case (pxa, _) => pxa
      }
    }
    img
      .grouped(dims._1)
      .map(row =>
        row.map {
          case 0 => ' '
          case 1 => '#'
          case x => throw new IllegalArgumentException(s"noo $x")
        }.mkString
      )
      .foreach(println)
  }

  println("--- testdata ---")
  run(testData, (2, 2))
  println("--- real ---")
  run(input, (25, 6))
}
