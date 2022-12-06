package twentytwo.six

import lib.Support

object TwentytwoSix extends App with Support {
  val testData =
    """
      |mjqjpqmgbljsphdztnvjfqwrcgsmlb
      |""".stripMargin.trim
  val input = load

  def run(data: String) = {
    def find(data: String, l: Int): Int = {
      data.sliding(l).zipWithIndex.collectFirst({
        case (cs, i) if cs.distinct.length == l => i + l
      }).get
    }

    println(find(data, 4))
    println(find(data, 14))
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
}
