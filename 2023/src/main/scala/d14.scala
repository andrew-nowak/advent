import lib.{Coord, Support}

import scala.annotation.tailrec

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

  def rot(maxX: Int)(z: Zone): Zone = z.map { case (c, t) => (c.ccw(maxX), t) }

  def spin(z: Zone): Zone = {
    val maxX = z.keySet.map(_.x).max
    val rt = rot(maxX) _
    val sn = send _
    Function.chain(List(sn, rt, sn, rt, sn, rt, sn, rt)).apply(z)
  }

  @tailrec def spinN(z: Zone, times: Long, seen: Map[Zone, Long]): Int = {
    seen.get(z) match {
      case Some(value) =>
        val period = times - value
        val pos = 1_000_000_000L % period
//        println(times, value, pos)
//        println(seen.toSeq.sortBy(_._2).map(q => countLoad(q._1)))
        countLoad(seen.find(_._2 == pos + value - 1).get._1)
      case None => spinN(spin(z), times + 1L, seen + (z -> (times + 1L)))
    }
  }

  def countLoad(z: Zone): Int = {
    val maxY = z.keySet.map(_.y).max
    z.collect { case (Coord(_, y), Rock) =>
      maxY + 1 - y
    }.sum
  }

  def run(data: String): Unit = {
    val start = System.nanoTime()

    val in = charCoords(data).collect {
      case (c, 'O') => (c, Rock)
      case (c, '#') => (c, Block)
    }

    val tiltedNorth = send(in)

//    printCoords[Tile](
//      LazyList.iterate(in)(spin).apply(2),
//      {
//        case Rock  => 'O'
//        case Block => '#'
//      }
//    )

    val loadNorth = countLoad(tiltedNorth)

    val p1 = loadNorth

    val p2 = spinN(in, 0L, Map.empty)

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
