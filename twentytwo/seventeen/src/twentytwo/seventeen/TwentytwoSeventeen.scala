package twentytwo.seventeen

import lib.{Coord, Support}

import scala.annotation.tailrec

object TwentytwoSeventeen extends App with Support {
  val testData =
    """
      |>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>
      |""".stripMargin.trim
  val input = load

  case class Rock private (blocks: Set[Coord], left: Int, right: Int) {
    def moveLeft: Rock = {
      if (left > 0) Rock(blocks.map(bl => bl.left), left - 1, right - 1)
      else this
    }

    def moveRight: Rock = {
      if (right < 6) Rock(blocks.map(bl => bl.right), left + 1, right + 1)
      else this
    }

    @tailrec final def moveDown(n: Int): Rock = {
      if (n <= 0) this
      else this.copy(blocks = blocks.map(_.up)).moveDown(n - 1) // y axis is inverted compared to Coord class
    }

    def setup(y: Int): Rock = {
      val sblocks = blocks.map(bl => Coord(bl.x + 2, bl.y + y + 4))
      Rock(sblocks, left + 2, right + 2)
    }
  }
  object Rock {
    def apply(blocks: Set[Coord]): Rock = {
      val xs = blocks.map(_.x)
      val left = xs.min
      val right = xs.max
      Rock(blocks, left, right)
    }
  }

  val rocks = LazyList
    .continually(
      Seq(
        Rock(Set(Coord(0, 0), Coord(1, 0), Coord(2, 0), Coord(3, 0))), // -
        Rock(Set(Coord(0, 1), Coord(1, 1), Coord(2, 1), Coord(1, 0), Coord(1, 2))), // +
        Rock(Set(Coord(0, 0), Coord(1, 0), Coord(2, 0), Coord(2, 1), Coord(2, 2))), // right angle
        Rock(Set(Coord(0, 0), Coord(0, 1), Coord(0, 2), Coord(0, 3))), // I
        Rock(Set(Coord(0, 0), Coord(0, 1), Coord(1, 0), Coord(1, 1))) // square
      )
    )
    .flatten

  case class State(blocks: Set[Coord], maxY: Int, jets: LazyList[Char], rocks: LazyList[Rock])

  def drop(state: State, n: Int, target: Int, startJets: List[Char], seenSlices: Map[Int, Int]): (State, Int) = {
    val jetsOffset = state.jets.take(startJets.size * 2).indexOfSlice(startJets)
    val newSeenSlices = if (state.rocks.head == rocks.head) {
      seenSlices + (jetsOffset -> n)
    } else {
      seenSlices
    }
    if (target == n) {
      state -> -1
    } else if (
      target == -1 && n != 0 && jetsOffset != 0 && state.rocks.head == rocks.head && seenSlices.contains(jetsOffset)
    ) {
      state -> (n - seenSlices(jetsOffset))
    } else {
      val jets = state.jets.take(4)
      val droppedRock = state.rocks.head.setup(state.maxY)
      val rock = jets
        .foldLeft(droppedRock) {
          case (rock, '<') => rock.moveLeft
          case (rock, '>') => rock.moveRight
        }
        .moveDown(3)
      drop(
        extendDrop(state.copy(jets = state.jets.drop(4), rocks = state.rocks.tail), rock),
        n + 1,
        target,
        startJets,
        newSeenSlices
      )
    }
  }

  @tailrec
  def extendDrop(state: State, rock: Rock): State = {
    val dropped = rock.moveDown(1)
    if (dropped.blocks.map(_.y).exists(_ <= 0) || dropped.blocks.exists(state.blocks.contains)) {
      // return rock
      state.copy(blocks = state.blocks ++ rock.blocks, maxY = Math.max(state.maxY, rock.blocks.map(_.y).max))
    } else {
      val shifted = state.jets.head match {
        case '<' => dropped.moveLeft
        case '>' => dropped.moveRight
      }
      if (shifted.blocks.exists(state.blocks.contains)) {
        extendDrop(state.copy(jets = state.jets.tail), dropped)
      } else {
        extendDrop(state.copy(jets = state.jets.tail), shifted)
      }
    }
  }

  def run(data: String) = {

    val startJets = data.trim.toCharArray

    val jets = LazyList.continually(startJets).flatten

    val initialState = State(Set.empty, 0, jets, rocks)

    val p1 = drop(initialState, 0, 2022, List.empty, Map.empty)
    println(p1._1.maxY)

    val period = drop(initialState, 0, -1, startJets.toList, Map.empty)._2

    val a = drop(initialState, 0, period, Nil, Map.empty)._1.maxY
    val b = drop(initialState, 0, period * 2, Nil, Map.empty)._1.maxY

    val risePerPeriod = b - a

    val target = 1_000_000_000_000L
    val remainder = target % period.toLong

    val runningRemainder = drop(initialState, 0, remainder.toInt, Nil, Map.empty)._1.maxY

    val answer = (target / period.toLong) * risePerPeriod.toLong + runningRemainder.toLong

    println(answer)

    println(period, risePerPeriod, remainder)
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
}
