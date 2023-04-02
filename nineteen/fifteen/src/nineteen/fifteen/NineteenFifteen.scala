package nineteen.fifteen

import lib.{Coord, Halt, Intcode, Support, WaitingForInput}
import scala.annotation.tailrec

case class State(intcode: Intcode, location: Coord, destination: Coord)

object NineteenFifteen extends App with Support {
  val input = loadLongSeq(",")

  def direction(from: Coord, to: Coord): Long = {
    if (from.north == to) 1
    else if (from.south == to) 2
    else if (from.west == to) 3
    else if (from.east == to) 4
    else throw new Exception(s"couldn't determine direction from $from to $to")
  }

  @tailrec
  def explore(
      toVisit: List[State],
      area: Map[Coord, Int],
      target: Option[Coord]
  ): (Map[Coord, Int], Option[Coord]) = {
    if (toVisit.isEmpty) (area, target)
    else {
      val head :: rest = toVisit
      val dir = direction(head.destination, head.location)
      val intcode = head.intcode.copy(input = List(dir))
      val outcome = Intcode.run(intcode) match {
        case Halt(intcode)            => throw new Exception("intcode unexpectedly halted")
        case WaitingForInput(intcode) => intcode
      }
      outcome.output.last match {
        case 0 => explore(rest, area + (head.destination -> 0), target)
        case x @ (1 | 2) =>
          val nextToVisit = rest ++ head.destination.cardinalNeighbours
            .filterNot(area.keySet.contains)
            .map(State(outcome, head.destination, _))

          val nextTarget = if (x == 1) target else Some(head.destination)
          explore(nextToVisit, area + (head.destination -> x.toInt), nextTarget)
      }
    }
  }

  def solve(area: Map[Coord, Int], origin: Coord): Map[Coord, Int] = {
    @tailrec def solveInner(fastest: Map[Coord, Int], next: List[Coord]): Map[Coord, Int] = {
      if (next.isEmpty) fastest
      else {
        val head :: rest = next
        val dist = head.cardinalNeighbours.flatMap(fastest.get).min + 1
        solveInner(
          fastest + (head -> dist),
          rest ++ head.cardinalNeighbours.filter(c => area(c) > 0 && !fastest.contains(c))
        )
      }
    }
    solveInner(Map(origin -> 0), origin.cardinalNeighbours.filter(area(_) > 0).toList)
  }

  val ic = new Intcode(Intcode.memoryFromSeq(input), 0)
  val initialLocation = Coord(0, 0)
  val initialArea = Map(initialLocation -> 3) // first square is walkable
  val initialToVisit = initialLocation.cardinalNeighbours.map(State(ic, initialLocation, _)).toList

  val (area, Some(target)) = explore(initialToVisit, initialArea, None)
  printCoords(
    area,
    {
      case 0 => '█'
      case 1 => ' '
      case 2 => '@'
      case 3 => '&'
    }
  )
  println("key: start: &; target: @")

  println(solve(area, Coord(0, 0))(target))
  println(solve(area, target).values.max)
}
