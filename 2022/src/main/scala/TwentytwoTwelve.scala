package twentytwo.twelve

import lib.{Coord, Support}

import scala.annotation.tailrec

final case class State(loc: Coord, steps: Int)

object TwentytwoTwelve extends App with Support {
//  val testData =
//    """
//      |Sabqponm
//      |abcryxxl
//      |accszExk
//      |acctuvwj
//      |abdefghi
//      |""".stripMargin.trim
  val testData =
    """
      |aaefgzklmzqrsvwxyzzzzzz
      |bcdzhijznopztuvwxzzzzzz
      |SzzzzzzzzzzzzzzzzzzzzzE
      |bzfghijklmnopqrstuvwxyz
      |cdezzzzzzzzzzzzzzzzzzzz
      |""".stripMargin.trim
  val input = load

  @tailrec
  def bfs(state: State, visited: Map[Coord, Int], q: List[State], target: Coord, heightmap: Map[Coord, Int]): Int = {
    if (visited.contains(target)) visited(target)
    else if (visited.get(state.loc).exists(_ <= state.steps) && q.isEmpty) Int.MaxValue
    else if (visited.get(state.loc).exists(_ <= state.steps)) bfs(q.head, visited, q.tail, target, heightmap)
    else {
      val nextSteps = state.loc.cardinalNeighbours
        .filter(heightmap.get(_).exists(_ <= heightmap(state.loc) + 1)) // can go to neighbour
        .filter(visited.get(_).forall(_ > state.steps + 1)) // don't add to queue if already visited faster
        .map(State(_, state.steps + 1))

      val nextq = (q ++ nextSteps).sortBy(st => st.steps + st.loc.manhattan(target))

      if (nextq.isEmpty) Int.MaxValue
      else bfs(nextq.head, visited + (state.loc -> state.steps), nextq.tail, target, heightmap)
    }
  }

  def run(data: String) = {
    val in = stringSeq(data)

    var start = Coord(-1, -1)
    var end = Coord(-1, -1)

    val heightmap = (for {
      (row, y) <- in.zipWithIndex
      (heightcode, x) <- row.zipWithIndex
    } yield {
      val height = heightcode match {
        case 'S' => start = Coord(x, y); 0
        case 'E' => end = Coord(x, y); 25
        case h   => h - 'a'
      }
      Coord(x, y) -> height
    }).toMap

    println(heightmap.size)

    val p1 = bfs(State(start, 0), Map.empty, Nil, end, heightmap)
    println(p1)

    val elevationAs = heightmap.filter(_._2 == 0).keys
    val p2 = elevationAs.map(a => bfs(State(a, 0), Map.empty, Nil, end, heightmap)).min

    println(p2)
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
}
