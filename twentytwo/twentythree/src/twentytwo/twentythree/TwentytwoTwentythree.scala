package twentytwo.twentythree

import lib.Support
import scala.annotation.tailrec
import lib.Coord
import lib.Direction
import lib.SeqExtras

case class Proposal(tests: Coord => Set[Coord], move: Coord => Coord)

object TwentytwoTwentythree extends App with Support {
  val testData =
    """
      |....#..
      |..###.#
      |#...#.#
      |.#...##
      |#.###..
      |##.#.##
      |.#..#..
      |""".stripMargin.trim
  val input = load

  val directions = Seq(
    Proposal(c => Set(c.north, c.nw, c.ne), c => c.north),
    Proposal(c => Set(c.south, c.sw, c.se), c => c.south),
    Proposal(c => Set(c.west, c.nw, c.sw), c => c.west),
    Proposal(c => Set(c.east, c.ne, c.se), c => c.east)
  )

  @tailrec
  def round(elves: Set[Coord], rounds: Int, p2: Boolean): (Set[Coord], Int) = {
    if (!p2 && rounds == 11) (elves, rounds)
    else {
      // println(rounds, elves)
      val proposals: Seq[(Coord, Option[Coord])] = elves.map { elf =>
        if (elf.neighbours.toSet.intersect(elves).isEmpty) {
          elf -> None
        } else {
          val proposalDirections = SeqExtras.rotate(directions, rounds % 4)

          val dir = proposalDirections.find(dir => dir.tests(elf).intersect(elves).isEmpty)

          elf -> dir.map(_.move(elf))
        }
      }.toSeq

      val afterMoveProposals = proposals.flatMap(_._2).groupBy(identity)

      if (afterMoveProposals.isEmpty) return (elves, rounds)

      val afterMoves = proposals.map {
        case (before, Some(after)) if afterMoveProposals(after).size == 1 => after
        case (before, _)                                                  => before
      }

      round(afterMoves.toSet, rounds + 1, p2)
    }
  }

  def score(elves: Set[Coord]): Int = {
    val x = elves.map(_.x)
    val y = elves.map(_.y)
    ((x.max - x.min + 1) * (y.max - y.min + 1)) - elves.size
  }

  def run(data: String) = {
    val elves = charCoords(data).collect { case (c, '#') =>
      c
    }

    val p1 = round(elves.toSet, 0, p2 = false)._1
    println(score(p1))

    val p2 = round(elves.toSet, 0, p2 = true)._2 + 1
    println(p2)
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
}
