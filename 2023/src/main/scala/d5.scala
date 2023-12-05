import lib.{Coord, Support}

object d5 extends App with Support {

  val testData =
    """seeds: 79 14 55 13
      |
      |seed-to-soil map:
      |50 98 2
      |52 50 48
      |
      |soil-to-fertilizer map:
      |0 15 37
      |37 52 2
      |39 0 15
      |
      |fertilizer-to-water map:
      |49 53 8
      |0 11 42
      |42 0 7
      |57 7 4
      |
      |water-to-light map:
      |88 18 7
      |18 25 70
      |
      |light-to-temperature map:
      |45 77 23
      |81 45 19
      |68 64 13
      |
      |temperature-to-humidity map:
      |0 69 1
      |1 0 69
      |
      |humidity-to-location map:
      |60 56 37
      |56 93 4
      |""".trim.stripMargin
  val input = load

  def run(data: String): Unit = {
    val in = stringSeq(data, delimiter = newline + newline)

    val seedsP1 = in.head.split(' ').flatMap(_.toLongOption)

    val stages: Seq[Long => Long] = in.tail.map { m =>
      m.split(newline)
        .tail
        .map { line =>
          val Array(dest, source, range) =
            line.split(' ').flatMap(_.toLongOption)
          val ret: PartialFunction[Long, Long] = {
            case n: Long if n >= source && n < source + range =>
              dest - source + n
          }
          ret
        }
        .foldLeft(PartialFunction.empty[Long, Long])(_ orElse _)
        .applyOrElse(_, identity[Long])
    }

    val location = Function.chain(stages)
    val locations = seedsP1.map(location)

    lazy val p1 = locations.min

    val seedsP2 = seedsP1
      .grouped(2)
      .map { case Array(start, range) =>
        (n: Long) => n >= start && n < start + range
      }
      .toSeq

    val stagesP2: Seq[Long => Long] = in.tail.map { m => (y: Long) =>
      m.split(newline)
        .tail
        .map { line =>
          val Array(dest, source, range) =
            line.split(' ').flatMap(_.toLongOption)
          val ret: PartialFunction[Long, Long] = {
            case n: Long if n >= dest && n < dest + range => source - dest + n
          }
          ret
        }
        .foldLeft(PartialFunction.empty[Long, Long])(_ orElse _)
        .applyOrElse(y, identity[Long])
    }.reverse

    val p2funcs = Function.chain(stagesP2)

    lazy val p2 = LazyList
      // get there quicker by doing bigger steps, eg .from(1, 10_000), and work down to smaller steps as you narrow down
      .from(1)
      .map(_.toLong)
      .filter(n => seedsP2.exists(_.apply(p2funcs(n))))
      .head

    println(p1)

    println(p2)
  }

  println("--- testdata ---")
  // run(testData)
  println("--- real ---")
  run(input)
}
