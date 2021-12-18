package twentyone.seventeen

import lib.Support

final case class State(x: Int, dx: Int, y: Int, dy: Int) {
  def iterate: State = State(x + dx, if (dx > 0) dx - 1 else 0, y + dy, dy - 1)
}
object TwentyoneSeventeen extends App with Support {
  val i = load

  val (minx, maxx, miny, maxy) = i match {
    case s"target area: x=${minx}..${maxx}, y=${miny}..${maxy}" => (minx.toInt, maxx.toInt, miny.toInt, maxy.toInt)
  }

  def sig(n: Int): Int = n * (n + 1) / 2

  def pathIntersects(ix: Int, iy: Int, minx: Int, maxx: Int, miny: Int, maxy: Int): Boolean = {
    val states = LazyList.iterate(State(0, ix, 0, iy))(_.iterate).takeWhile(_.y >= miny)
    states.exists(state => state.x >= minx && state.x <= maxx && state.y >= miny && state.y <= maxy)
  }

  val highest = (for {
    ix <- 0 to maxx
    if sig(ix) >= minx && sig(ix) <= maxx
    iy <- (0 to miny.abs).reverse
    if pathIntersects(ix, iy, minx, maxx, miny, maxy)
  } yield (ix, iy)).maxBy(_._2)._2

  val part1 = sig(highest)
  println(part1)

  val all = for {
    ix <- 0 to maxx
    iy <- miny to miny.abs
    if pathIntersects(ix, iy, minx, maxx, miny, maxy)
  } yield (ix, iy)

  println(all.size)
}
