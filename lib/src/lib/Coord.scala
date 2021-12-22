package lib

case class Coord(x: Int, y: Int) {
  def cardinalNeighbours: Seq[Coord] = Seq(
    Coord(x - 1, y),
    Coord(x + 1, y),
    Coord(x, y - 1),
    Coord(x, y + 1)
  )

  /** Neighbours not including self
    * @return
    */
  def neighbours: Seq[Coord] = surrounding.filter(coord => x != coord.x || y != coord.y)

  /** Includes self, ordered from top left to bottom right, travelling horizontally
    * @return
    */
  def surrounding: Seq[Coord] = for {
    ny <- (y - 1) to (y + 1)
    nx <- (x - 1) to (x + 1)
  } yield Coord(nx, ny)

  def manhattan(to: Coord): Int = (x - to.x).abs + (y - to.y).abs

  def inBounds: Boolean = x >= 0 && y >= 0

  def inBounds(max: Coord): Boolean = inBounds && x <= max.y && y <= max.y
}

case class Coord3d(x: Int, y: Int, z: Int)
