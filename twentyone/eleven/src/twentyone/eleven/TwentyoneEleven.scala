package twentyone.eleven

import lib.{Coord, Support}

import scala.annotation.tailrec

final case class State(energies: Map[Coord, Int], flashes: Int = 0)

object TwentyoneEleven extends App with Support {

  @tailrec def countFlashes(hasFlashed: Set[Coord], energies: Map[Coord, Int]): (Map[Coord, Int], Int) = {
    val flashes = energies.filter(_._2 > 9).keys.toSet.diff(hasFlashed)
    if (flashes.isEmpty) {
      val finalEnergies = energies.map {
        case (loc, energy) if energy > 9 => (loc, 0)
        case otherwise                   => otherwise
      }
      (finalEnergies, hasFlashed.size)
    } else {
      val energyIncreases = flashes.toList.flatMap(_.neighbours)
      val nextEnergies =
        energyIncreases.foldLeft(energies)((energyMap, coord) => energyMap.updatedWith(coord)(_.map(1.+)))
      countFlashes(hasFlashed ++ flashes, nextEnergies)
    }
  }
  def stepCumulative(state: State): State = {
    val energiesIncremented = state.energies.map(e => (e._1, e._2 + 1))
    val count = countFlashes(Set.empty, energiesIncremented)
    State(count._1, state.flashes + count._2)
  }

  def step(state: State): State = {
    val energiesIncremented = state.energies.map(e => (e._1, e._2 + 1))
    val count = countFlashes(Set.empty, energiesIncremented)
    State(count._1, count._2)

  }

  val initialEnergies = load2dIntSeqWithCoords(delimiterB = "")

  val part1 = LazyList.iterate(State(initialEnergies))(stepCumulative).apply(100).flashes
  println(part1)

  val part2 =
    LazyList.iterate(State(initialEnergies))(step).zipWithIndex.find { case (state, _) => state.flashes == 100 }.get._2
  println(part2)
}
