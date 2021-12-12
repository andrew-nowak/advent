package twentyone.twelve

import lib.Support

import scala.annotation.tailrec

object TwentyoneTwelve extends App with Support {
  val i = loadStringSeq.map(_.split("-")).map(sp => (sp.head, sp.last))

  val bidi = (i ++ i.map(_.swap)).groupBy(_._1).view.mapValues(_.map(_._2)).toMap

  final case class State(location: String, visited: Set[String], spareTime: Boolean = false)

  @tailrec
  def traverse(q: List[State], paths: Int): Int = {
    q match {
      case Nil => paths
      case head :: rest if head.location == "end" =>
        traverse(rest, paths + 1)
      case head :: rest =>
        val nextStates = bidi(head.location).flatMap { cave =>
          if (!(head.visited.contains(cave) && cave.toLowerCase == cave))
            Some(State(cave, head.visited + head.location, head.spareTime))
          else if (head.spareTime && cave != "start")
            Some(State(cave, head.visited + head.location, false))
          else
            None
        }
        traverse(rest ++ nextStates, paths)
    }
  }

  val initialState = State("start", Set.empty)

  val part1 = traverse(List(initialState), 0)
  println(part1)

  val part2 = traverse(List(initialState.copy(spareTime = true)), 0)
  println(part2)
}
