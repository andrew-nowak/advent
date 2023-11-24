package nineteen.eleven

import lib.{AsyncIntcode, Coord, Provider, Support}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

sealed trait Direction {
  def left: Direction
  def right: Direction
  def forward(coord: Coord): Coord
}
object North extends Direction {
  override def left: Direction = West
  override def right: Direction = East
  override def forward(coord: Coord): Coord = Coord(coord.x, coord.y + 1)
}
object East extends Direction {
  override def left: Direction = North
  override def right: Direction = South
  override def forward(coord: Coord): Coord = Coord(coord.x + 1, coord.y)
}
object South extends Direction {
  override def left: Direction = East
  override def right: Direction = West
  override def forward(coord: Coord): Coord = Coord(coord.x, coord.y - 1)
}
object West extends Direction {
  override def left: Direction = South
  override def right: Direction = North
  override def forward(coord: Coord): Coord = Coord(coord.x - 1, coord.y)
}

class Robot(instructions: Seq[Long], starting: Boolean) {
  var direction: Direction = North
  var hull = if (starting) Map(Coord(0, 0) -> true) else Map.empty[Coord, Boolean]
  var location = Coord(0, 0)

  def paints: Int = hull.size

  def provideInput: Future[Long] = {
    Future.successful(
      if (hull.getOrElse(location, false))
        1L
      else
        0L
    )
  }

  var painting = true
  def give(out: Long): Unit = {
    (painting, out) match {
      case (true, 0)  => hull = hull + (location -> false)
      case (true, _)  => hull = hull + (location -> true)
      case (false, 0) => direction = direction.left
      case (false, _) => direction = direction.right
    }
    if (!painting) location = direction.forward(location)
    painting = !painting
  }

  val completion = AsyncIntcode.buildAndRunWithBuffers(
    instructions,
    new Provider { override def take: Future[Long] = provideInput },
    give
  )
}

object NineteenEleven extends App with Support {
  val input = load

  def run(data: String) = {
    val in = longSeq(data, ",")

    val p1 = new Robot(in, false)
    Await.result(p1.completion, 5.seconds)
    println(p1.paints)

    val p2 = new Robot(in, true)
    Await.result(p2.completion, 5.seconds)
    printCoords(p2.hull.filter(_._2).keys.toSeq)
  }
  println("--- real ---")
  run(input)
}
