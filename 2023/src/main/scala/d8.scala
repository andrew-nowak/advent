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

  private val networkR = """^([A-Z12]{3}) = \(([A-Z12]{3}), ([A-Z12]{3})\)$""".r

  def run(data: String): Unit = {
    val start = System.nanoTime()
    val in = stringSeq(data)

    val dirs: LazyList[Char] = LazyList.continually(in.head.toCharArray).flatten

    val network: Map[String, (String, String)] =
      in.collect { case networkR(location, l, r) => location -> (l -> r) }.toMap

    @tailrec def findZZZ(dirs: LazyList[Char], loc: String, time: Int): Int = {
      if (loc == "ZZZ") time
      else {
        val (left, right) = network(loc)
        dirs match {
          case 'L' #:: nextDirs => findZZZ(nextDirs, left, time + 1)
          case 'R' #:: nextDirs => findZZZ(nextDirs, right, time + 1)
        }
      }
    }

    lazy val p1 =
      if (network.keySet.contains("AAA")) findZZZ(dirs, "AAA", 0) else -1

    @tailrec def findPeriod(
        dirs: LazyList[Char],
        loc: String,
        time: Long,
        visited: Map[String, Long]
    ): Long = {
      if (visited.contains(loc) && loc.endsWith("Z")) {
        val period = time - visited(loc)
        println(
          s"returned to $loc on $time, last time round at ${visited(loc)}, for period $period"
        )
        period
      } else {
        val (left, right) = network(loc)
        dirs match {
          case 'L' #:: nextDirs =>
            findPeriod(nextDirs, left, time + 1, visited + (loc -> time))
          case 'R' #:: nextDirs =>
            findPeriod(nextDirs, right, time + 1, visited + (loc -> time))
        }
      }
    }
    lazy val p2 = lcm(
      network.keySet
        .filter(_.endsWith("A"))
        .map(findPeriod(dirs, _, 0L, Map.empty))
    )

    println(p1)

    println(p2)

    val end = System.nanoTime()
    println(s"Done in ${(end - start).toDouble / 1_000_000} ms")
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
}
