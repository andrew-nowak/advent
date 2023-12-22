package lib

sealed trait Direction {
  def cw: Direction
  def ccw: Direction
  def rev: Direction
}
case object Direction {
  case object Up extends Direction {
    def cw = Right
    def ccw = Left
    def rev = Down
  }
  case object Down extends Direction {
    def cw = Left
    def ccw = Right
    def rev = Up
  }
  case object Left extends Direction {
    def cw = Up
    def ccw = Down
    def rev = Right
  }
  case object Right extends Direction {
    def cw = Down
    def ccw = Up
    def rev = Left
  }
  lazy val all: Set[Direction] = Set(Up, Down, Left, Right)
}

case class Coord(x: Int, y: Int) {
  def cardinalNeighbours: Seq[Coord] = Seq(
    left,
    right,
    up,
    down
  )

  def up(n: Int): Coord = Coord(x, y - n)
  def up: Coord = up(1)
  def down(n: Int): Coord = Coord(x, y + n)
  def down: Coord = down(1)
  def left(n: Int): Coord = Coord(x - n, y)
  def left: Coord = left(1)
  def right(n: Int): Coord = Coord(x + n, y)
  def right: Coord = right(1)

  def north: Coord = up
  def south: Coord = down
  def west: Coord = left
  def east: Coord = right
  def nw: Coord = Coord(x - 1, y - 1)
  def ne: Coord = Coord(x + 1, y - 1)
  def sw: Coord = Coord(x - 1, y + 1)
  def se: Coord = Coord(x + 1, y + 1)

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

  def cw(maxY: Int): Coord = Coord(y, -x + maxY)
  def ccw(maxX: Int): Coord = Coord(-y + maxX, x)

  def go(dir: Direction): Coord = dir match {
    case Direction.Up    => up
    case Direction.Down  => down
    case Direction.Left  => left
    case Direction.Right => right
  }

  def go(dir: Direction, n: Int): Coord = dir match {
    case Direction.Up    => up(n)
    case Direction.Down  => down(n)
    case Direction.Left  => left(n)
    case Direction.Right => right(n)
  }
}

object Origin extends Coord(0, 0)

case class Coord3d(x: Int, y: Int, z: Int) {
  def manhattan: Int = x.abs + y.abs + z.abs
  def +(o: Coord3d): Coord3d = Coord3d(x + o.x, y + o.y, z + o.z)

  def surrounding: Seq[Coord3d] = for {
    nz <- (z - 1) to (z + 1)
    ny <- (y - 1) to (y + 1)
    nx <- (x - 1) to (x + 1)
  } yield Coord3d(nx, ny, nz)

  def cardinalNeighbours: Seq[Coord3d] = Seq(
    Coord3d(x + 1, y, z),
    Coord3d(x - 1, y, z),
    Coord3d(x, y + 1, z),
    Coord3d(x, y - 1, z),
    Coord3d(x, y, z + 1),
    Coord3d(x, y, z - 1)
  )

  override def toString: String = (x, y, z).toString()
}
