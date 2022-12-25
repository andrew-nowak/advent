package twentytwo.twentyfour

import lib.Support
import lib.Coord
import lib.Direction
import scala.annotation.tailrec

case class State(bounds: (Int, Int), location: Coord, blizzards: List[Blizzard], steps: Int) {
  val target = Coord(bounds._1 - 1, bounds._2)
}
case class Blizzard(location: Coord, direction: Direction)

object TwentytwoTwentyfour extends App with Support {
  val testData =
    """
      |#.######
      |#>>.<^<#
      |#.<..<<#
      |#>v.><>#
      |#<^v^^>#
      |######.#
      |""".stripMargin.trim
  val input = load

  @tailrec def bfs(q: List[State], seen: Set[(Coord, Int)], targets: List[Coord]): Int = q match {
    case Nil                                                           => Int.MaxValue
    case state :: rest if seen.contains((state.location, state.steps)) => bfs(rest, seen, targets)
    case state :: rest                                                 =>
      // println("---")
      val nextBlizzards = state.blizzards.map {
        case Blizzard(Coord(x, y), Direction.Right) =>
          val nx = if (x + 1 >= state.bounds._1) 1 else x + 1
          Blizzard(Coord(nx, y), Direction.Right)
        case Blizzard(Coord(x, y), Direction.Left) =>
          val nx = if (x - 1 < 1) state.bounds._1 - 1 else x - 1
          Blizzard(Coord(nx, y), Direction.Left)
        case Blizzard(Coord(x, y), Direction.Up) =>
          val ny = if (y - 1 < 1) state.bounds._2 - 1 else y - 1
          Blizzard(Coord(x, ny), Direction.Up)
        case Blizzard(Coord(x, y), Direction.Down) =>
          val ny = if (y + 1 >= state.bounds._2) 1 else y + 1
          Blizzard(Coord(x, ny), Direction.Down)
      }
      val blizzlocs = nextBlizzards.map(_.location).toSet
      val nbors = state.location.cardinalNeighbours
        .filter(c => c.x > 0 && c.x < state.bounds._1 && c.y > 0 && c.y < state.bounds._2)
      val nextSteps = (nbors :+ state.location).filterNot(blizzlocs.contains)
      val targ = targets.head
      if (state.location.cardinalNeighbours.contains(targ)) {
        if (targets.size == 1) {
          state.steps + 1
        } else {
          bfs(
            List(state.copy(location = targ, blizzards = nextBlizzards, steps = state.steps + 1)),
            seen = Set.empty,
            targets = targets.tail
          )
        }
      } else {
        val nextStates =
          rest ++ nextSteps.map(step => state.copy(blizzards = nextBlizzards, steps = state.steps + 1, location = step))
        bfs(
          nextStates.sortBy(state => state.steps + state.location.manhattan(targ)),
          seen + ((state.location, state.steps)),
          targets
        )
      }
  }

  def run(data: String) = {
    val entry = Coord(1, 0)
    val in = charCoords(data)

    val maxX = in.keys.map(_.x).max
    val maxY = in.keys.map(_.y).max
    println(maxX, maxY)

    val exit = Coord(maxX - 1, maxY)

    val blizzards = in.collect {
      case (c, '^') => Blizzard(c, Direction.Up)
      case (c, '>') => Blizzard(c, Direction.Right)
      case (c, 'v') => Blizzard(c, Direction.Down)
      case (c, '<') => Blizzard(c, Direction.Left)
    }.toList

    val initialState = State((maxX, maxY), entry, blizzards, 0)

    val p1 = bfs(List(initialState), Set.empty, targets = List(Coord(maxX - 1, maxY)))
    println(p1)
    val p2 = bfs(List(initialState), Set.empty, targets = List(exit, entry, exit))
    println(p2)
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
}
