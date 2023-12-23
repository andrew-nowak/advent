import lib._

import scala.annotation.tailrec

object d23 extends App with Support {

  val testData =
    """#.#####################
      |#.......#########...###
      |#######.#########.#.###
      |###.....#.>.>.###.#.###
      |###v#####.#v#.###.#.###
      |###.>...#.#.#.....#...#
      |###v###.#.#.#########.#
      |###...#.#.#.......#...#
      |#####.#.#.#######.#.###
      |#.....#.#.#.......#...#
      |#.#####.#.#.#########v#
      |#.#...#...#...###...>.#
      |#.#.#v#######v###.###v#
      |#...#.>.#...>.>.#.###.#
      |#####v#.#.###v#.#.###.#
      |#.....#...#...#.#.#...#
      |#.#########.###.#.#.###
      |#...###...#...#...#.###
      |###.###.#.###v#####v###
      |#...#...#.#.>.>.#.>.###
      |#.###.###.#.###.#.#v###
      |#.....###...###...#...#
      |#####################.#""".stripMargin.trim
  val input = load

  def canMove(m: Map[Coord, Char], move: Coord, dirTile: Char): Option[Coord] =
    if (m.get(move).exists(c => c == '.' || c == dirTile)) Some(move) else None

  final case class State(loc: Coord, path: Set[Coord])

  @tailrec def search(
      q: List[State],
      m: Map[Coord, Char],
      dest: Coord,
      longest: Int
  ): Int =
    q match {
      case Nil => longest
      case State(loc, path) :: t if loc == dest =>
        search(t, m, dest, math.max(longest, path.size - 1))
      case State(loc, path) :: t =>
//        println(loc)
        val moves = Set(
          canMove(m, loc.up, '^'),
          canMove(m, loc.left, '<'),
          canMove(m, loc.right, '>'),
          canMove(m, loc.down, 'v')
        ).flatten.diff(path).toList
        val ns = moves.map(c => State(c, path + c))
        search(ns ::: t, m, dest, longest)
    }

  def findNeighbours(
      junc: (Coord, Char),
      allJuncs: Map[Coord, Char],
      m: Set[Coord]
  ): (Char, Map[Char, Int]) = {
    val juncs = allJuncs - junc._1
    @tailrec def inner(
        q: List[State],
        routes: Seq[(Char, Int)]
    ): Seq[(Char, Int)] =
      q match {
        case Nil => routes
        case State(loc, path) :: t if juncs.keySet contains loc =>
          inner(t, routes :+ (juncs(loc), path.size - 1))
        case State(loc, path) :: t =>
          val nextSteps =
            loc.cardinalNeighbours.toSet.intersect(m).diff(path).toList
          val nextStates = nextSteps.map(c => State(c, path + c))
          inner(t ::: nextStates, routes)
      }
    junc._2 -> inner(List(State(junc._1, Set(junc._1))), Seq.empty).toMap
  }

  final case class State2(loc: Char, visited: Set[Char], steps: Int)
  @tailrec def runGraph(
      q: List[State2],
      g: Map[Char, Map[Char, Int]],
      dest: Char,
      longest: Int
  ): Int = {
    q match {
      case Nil => longest
      case State2(loc, visited, steps) :: t if loc == dest =>
        if (steps > longest) println(steps, visited)
        runGraph(t, g, dest, longest = math.max(longest, steps))
      case State2(loc, visited, steps) :: t =>
        val moves = g(loc) -- visited
        val newStates = moves.map { case (c, i) =>
          State2(c, visited + c, steps + i)
        }.toList
        runGraph(newStates ++ t, g, dest, longest)
    }
  }

  def run(data: String): Unit = {
    val startTime = System.nanoTime()

    val in = charCoords(data)
    val directionalPath = in.filterNot(_._2 == '#')

    val start = directionalPath.minBy(_._1.manhattan(Origin))._1
    val end = directionalPath.maxBy(_._1.manhattan(Origin))._1

    val startState = State(start, Set(start))
    val p1 = search(List(startState), directionalPath, end, 0)

    println(p1)

    val path = directionalPath.keySet
    val junctions = (path
      .filter(_.cardinalNeighbours.toSet.intersect(path).size >= 3)
      .toSeq
      .zipWithIndex
      .map { case (coord, i) => (coord, (i + 'A').toChar) } ++ Seq(
      (start, '0'),
      (end, '1')
    )).toMap

    val graph: Map[Char, Map[Char, Int]] =
      junctions.map(findNeighbours(_, junctions, path))

    println(graph.size, graph)
//    printCoords[Char](path.map((_, '█')).toMap ++ junctions, {
//      case n: Char => n
//    }, default = ' ')

    println(graph)

    val initialState = State2('0', Set('0'), 0)
    val p2 = runGraph(List(initialState), graph, '1', -1)
    // in.size // searchAgain(List(startState), path.keySet.map((_, 0)).toMap, end, 0)

    println(p2)

    val endTime = System.nanoTime()
    println(s"Done in ${(endTime - startTime).toDouble / 1_000_000} ms")
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
}
