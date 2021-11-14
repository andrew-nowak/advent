package nineteen.three

import lib.Support

final case class Coord(x: Int, y: Int) {
  def up(dist: Int = 1): Coord = Coord(x, y + dist)
  def down(dist: Int = 1): Coord = Coord(x, y - dist)
  def left(dist: Int = 1): Coord = Coord(x - dist, y)
  def right(dist: Int = 1): Coord = Coord(x + dist, y)
  def manhattan: Int = x.abs + y.abs
}

final case class State(seen: Map[Coord, Int], last: Coord, n: Int)

object Three extends App with Support {
  val origin = Coord(0, 0)

  def wire(path: Seq[String]): Map[Coord, Int] = {
    path
      .foldLeft(State(seen = Map(origin -> 0), last = origin, n = 0)) {
        (state, inst) =>
          val dir = inst.head
          val dist = inst.tail.toInt
          val travelled: Seq[(Coord, Int)] = (1 to dist).map(d =>
            (dir match {
              case 'U' => state.last.up(d)
              case 'D' => state.last.down(d)
              case 'L' => state.last.left(d)
              case 'R' => state.last.right(d)
            }) -> (state.n + d)
          )
          State(
            seen = state.seen ++ travelled.toMap,
            last = travelled.last._1,
            n = state.n + dist
          )
      }
      .seen
  }

  def part1(wires: Seq[Map[Coord, Int]]): Int = {
    val crossings = wires.map(_.keySet).reduce(_ intersect _)
    crossings.map(_.manhattan).filterNot(_ == 0).min
  }

  def part2(wires: Seq[Map[Coord, Int]]): Int = {
    val crossings = wires.map(_.keySet).reduce(_ intersect _)
    crossings.map(loc => wires.flatMap(_.get(loc)).sum).filterNot(_ == 0).min
  }

  val in = loadStringSeq
  val paths = in.map(_.split(","))
  val wires = paths.map(p => wire(p))

  println(part1(wires))
  println(part2(wires))
}
