import lib.Direction.{Down, Right}
import lib.{Coord, Direction, Support}

import scala.annotation.tailrec
import scala.collection.mutable

object d17 extends App with Support {
  val testData =
    """
      |2413432311323
      |3215453535623
      |3255245654254
      |3446585845452
      |4546657867536
      |1438598798454
      |4457876987766
      |3637877979653
      |4654967986887
      |4564679986453
      |1224686865563
      |2546548887735
      |4322674655533""".stripMargin.trim
  val input = load

  final case class State(
      pos: Coord,
      dir: Direction,
      times: Int,
      heatLoss: Int
  ) {
    val visit: (Coord, Direction, Int) = (pos, dir, times)
    def priority(dest: Coord) = heatLoss + pos.manhattan(dest)
  }

  @tailrec def astarStandard(
      q: mutable.PriorityQueue[State],
      m: Map[Coord, Int],
      dest: Coord,
      visited: Set[(Coord, Direction, Int)]
  ): Int = {
    val head = q.dequeue()
    if (head.pos == dest) head.heatLoss
    else if (visited contains head.visit) astarStandard(q, m, dest, visited)
    else {
      val nextDs =
        Direction.all - head.dir.rev diff (if (head.times == 3) Set(head.dir)
                                           else Set.empty)
      val newStates = nextDs.toSeq.map(d => d -> head.pos.go(d)).collect {
        case (d, pos) if pos.inBounds(dest) =>
          State(
            pos = pos,
            dir = d,
            times = if (d == head.dir) head.times + 1 else 1,
            heatLoss = head.heatLoss + m(pos)
          )
      }

      q.enqueue(newStates: _*)

      astarStandard(q, m, dest, visited + head.visit)
    }
  }

  @tailrec def astarUltra(
      q: mutable.PriorityQueue[State],
      m: Map[Coord, Int],
      dest: Coord,
      visited: Set[(Coord, Direction, Int)]
  ): Int = {
    val head = q.dequeue()
    if (head.pos == dest && head.times >= 4) head.heatLoss
    else if (visited contains head.visit) astarUltra(q, m, dest, visited)
    else {
      val nextDs =
        if (head.times < 4) Set(head.dir)
        else
          Direction.all - head.dir.rev diff (if (head.times == 10) Set(head.dir)
                                             else Set.empty)
      val newStates = nextDs.toSeq.map(d => d -> head.pos.go(d)).collect {
        case (d, pos) if pos.inBounds(dest) =>
          State(
            pos = pos,
            dir = d,
            times = if (d == head.dir) head.times + 1 else 1,
            heatLoss = head.heatLoss + m(pos)
          )
      }

      q.enqueue(newStates: _*)

      astarUltra(q, m, dest, visited + head.visit)
    }
  }

  def run(data: String): Unit = {
    val start = System.nanoTime()

    val in = `2dIntSeqWithCoords`(data, delimiterB = "")

    val maxX = in.map(_._1.x).max
    val maxY = in.map(_._1.y).max
    val dest = Coord(maxX, maxY)

    val initialStates = List(Down, Right).map(d =>
      State(pos = Coord(0, 0), dir = d, times = 1, heatLoss = 0)
    )
    val pq = mutable.PriorityQueue.from(initialStates) { (x: State, y: State) =>
      -(x.priority(dest) compare y.priority(dest))
    }
    val p1 = astarStandard(pq.clone(), in, dest, Set.empty)
    println(p1)

    val p2 = astarUltra(pq.clone(), in, dest, Set.empty)
    println(p2)

    val end = System.nanoTime()
    println(s"Done in ${(end - start).toDouble / 1_000_000} ms")
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)

}
