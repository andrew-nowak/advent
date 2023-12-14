import lib.{Coord, Support}

object d14 extends App with Support {
  sealed trait Tile
  object Rock extends Tile
  object Block extends Tile
  val testData =
    """
      |O....#....
      |O.OO#....#
      |.....##...
      |OO.#O....O
      |.O.....O#.
      |O.#..O.#.#
      |..O..#O..O
      |.......O..
      |#....###..
      |#OO..#....
      |""".trim.stripMargin
  val input = load

  private type Zone = Map[Coord, Tile]

  def send(m: Zone): Zone = {
    val cols = m.keySet.map(_.x).max
    (0 to cols)
      .flatMap(col => {
        val colItems = m.filter(_._1.x == col).toSeq.sortBy(_._1.y)
        colItems.foldLeft(List.empty[(Coord, Tile)]) {
          case (acc, a @ (_, Block))      => a :: acc
          case (Nil, (Coord(x, _), Rock)) => (Coord(x, 0), Rock) :: Nil
          case (acc @ (Coord(_, ya), _) :: _, (Coord(x, _), Rock)) =>
            (Coord(x, ya + 1), Rock) :: acc
        }
      })
      .toMap
  }

  def run(data: String): Unit = {
    val start = System.nanoTime()

    val in = charCoords(data).collect {
      case (c, 'O') => (c, Rock)
      case (c, '#') => (c, Block)
    }

    val tiltedNorth = send(in)

    printCoords[Tile](
      tiltedNorth,
      {
        case Rock  => 'O'
        case Block => '#'
      }
    )

    val maxY = in.keySet.map(_.y).max
    val loadNorth = tiltedNorth.collect { case (Coord(_, y), Rock) =>
      maxY + 1 - y
    }.sum

    val p1 = loadNorth

    val p2 = in.size

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
