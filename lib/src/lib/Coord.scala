package lib

case class Coord(x: Int, y: Int) {
  def cardinalNeighbours: Seq[Coord] = Seq(
    Coord(x - 1, y),
    Coord(x + 1, y),
    Coord(x, y - 1),
    Coord(x, y + 1)
  )

  def neighbours: Seq[Coord] = for {
    nx <- (x - 1) to (x + 1)
    ny <- (y - 1) to (y + 1) if x != nx || y != ny
  } yield Coord(nx, ny)

  def manhattan(to: Coord): Int = (x - to.x).abs + (y - to.y).abs

  def inBounds: Boolean = x >= 0 && y >= 0

  def inBounds(max: Coord): Boolean = inBounds && x <= max.y && y <= max.y
}
