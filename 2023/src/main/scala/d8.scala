import lib.Support

import scala.annotation.tailrec

object d8 extends App with Support {

  val testData =
    """
      |LR
      |
      |11A = (11B, XXX)
      |11B = (XXX, 11Z)
      |11Z = (11B, XXX)
      |22A = (22B, XXX)
      |22B = (22C, 22C)
      |22C = (22Z, 22Z)
      |22Z = (22B, 22B)
      |XXX = (XXX, XXX)
      |""".trim.stripMargin
  val input = load

  val networkR = """^([A-Z12]{3}) = \(([A-Z12]{3}), ([A-Z12]{3})\)$""".r

  def run(data: String): Unit = {
    val in = stringSeq(data)

    val steps = LazyList.continually(in.head.toCharArray).flatten

    val network = in
      .drop(2)
      .map { case networkR(location, l, r) => location -> (l -> r) }
      .toMap

    @tailrec def findZZZ(steps: LazyList[Char], at: String, taken: Int): Int = {
      if (at == "ZZZ") taken
      else
        steps match {
          case 'L' #:: nextDirs => findZZZ(nextDirs, network(at)._1, taken + 1)
          case 'R' #:: nextDirs => findZZZ(nextDirs, network(at)._2, taken + 1)
        }
    }

    lazy val p1 =
      if (network.keySet.contains("AAA")) findZZZ(steps, "AAA", 0) else -1

    @tailrec def findPeriod(
        dirs: LazyList[Char],
        at: String,
        taken: Long,
        visited: Map[String, Long]
    ): Long = {
      if (visited.contains(at) && at.endsWith("Z")) {
        val period = taken - visited(at)
        println(
          s"returned to $at on $taken, last time round at ${visited(at)}, for period $period"
        )
        period
      } else {
        dirs match {
          case 'L' #:: nextDirs =>
            findPeriod(
              nextDirs,
              network(at)._1,
              taken + 1,
              visited + (at -> taken)
            )
          case 'R' #:: nextDirs =>
            findPeriod(
              nextDirs,
              network(at)._2,
              taken + 1,
              visited + (at -> taken)
            )
        }
      }
    }
    lazy val p2 = lcm(
      network.keySet
        .filter(_.endsWith("A"))
        .toSeq
        .map(findPeriod(steps, _, 0L, Map.empty))
    )

    println(p1)

    println(p2)
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
}
