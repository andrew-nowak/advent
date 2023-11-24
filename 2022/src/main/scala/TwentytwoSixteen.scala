package twentytwo.sixteen

import lib.Support

import scala.annotation.tailrec

object TwentytwoSixteen extends App with Support {
  val parseR = """Valve ([A-Z]{2}) has flow rate=(\d+); tunnels? leads? to valves? (.*)""".r
  val testData =
    """
      |Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
      |Valve BB has flow rate=13; tunnels lead to valves CC, AA
      |Valve CC has flow rate=2; tunnels lead to valves DD, BB
      |Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
      |Valve EE has flow rate=3; tunnels lead to valves FF, DD
      |Valve FF has flow rate=0; tunnels lead to valves EE, GG
      |Valve GG has flow rate=0; tunnels lead to valves FF, HH
      |Valve HH has flow rate=22; tunnel leads to valve GG
      |Valve II has flow rate=0; tunnels lead to valves AA, JJ
      |Valve JJ has flow rate=21; tunnel leads to valve II
      |""".stripMargin.trim
  val input = load

  case class Valve(name: String, flow: Int, tunnels: Seq[String])
  case class State(location: String, openValves: Set[String], time: Int, pressure: Int)

  def run(data: String): Unit = {
    val in = stringSeq(data)
    val valveLookup = in.map { case parseR(name, flow, tunnels) =>
      name -> Valve(name, flow.toInt, tunnels.split(", ").toSeq)
    }.toMap
    val valves = valveLookup.values.toSet
    val flowableValves = valves.filter(_.flow > 0).map(_.name)
    val flowableValvesSize = flowableValves.size
    val start = "AA"
    val initialState = State(start, Set.empty, 0, 0)

    val paths = (flowableValves.toSeq.combinations(2) ++ flowableValves.toSeq.map(Seq("AA", _))).flatMap {
      case Seq(valveA, valveB) =>
        @tailrec def route(state: List[(String, Seq[String])]): Seq[String] = {
          if (state.head._1 == valveB) state.head._2 :+ valveB
          else {
            val next = valveLookup(state.head._1).tunnels.filterNot(state.head._2.contains)

            val q = (state.tail ++ next.map(_ -> (state.head._2 :+ state.head._1))).sortBy(_._2.size)
            route(q)
          }
        }

        val path = route(List(valveA -> Seq.empty))
        Seq((valveA, valveB) -> path, (valveB, valveA) -> path)
    }.toMap

    def doTheSearch(initialState: State, availableValves: Set[String], maxTime: Int): State = {
      def flow(state: State): Int = state.openValves.map(valveLookup).map(_.flow).sum
      @tailrec def search(q: List[State], best: State): State = {
        if (q.isEmpty) best
        else {
          val state = q.head
          val closedValves = availableValves.diff(state.openValves)
          if (state.time == maxTime) {
            search(q.tail, if (state.pressure > best.pressure) state else best)
          } else if (state.time > maxTime) {
            throw new Error(s"exceeded time!! $state")
          } else if (closedValves.isEmpty) {
            search(
              state.copy(time = maxTime, pressure = state.pressure + flow(state) * (maxTime - state.time)) +: q.tail,
              best
            )
          } else {
            val newStates = closedValves.flatMap(cv => {
              val dist = paths(state.location -> cv).size
              if (state.time + dist > maxTime) None
              else {
                val st = state.copy(
                  location = cv,
                  openValves = state.openValves + cv,
                  time = state.time + dist,
                  pressure = state.pressure + flow(state) * dist
                )
                Some(st)
              }
            })
            val noMoreMovesState = state.copy(
              time = maxTime,
              pressure = state.pressure + flow(state) * (maxTime - state.time)
            )
            search(noMoreMovesState :: newStates.toList ::: q.tail, best)
          }
        }
      }
      search(List(initialState), State(location = "", openValves = Set.empty, time = 0, pressure = 0))
    }

    val p1 = doTheSearch(initialState, flowableValves, 30)
    println(p1.pressure)

    val fvs = flowableValves.toSeq
    val responsibilities =
      (0 to flowableValvesSize / 2).flatMap(n => fvs.combinations(n)).map(_.toSet).map(l => (l, flowableValves.diff(l)))
    println(s"responsibility possibilities: ${responsibilities.size}")

    val p2 = responsibilities.zipWithIndex.map { case ((myValves, elephantValves), i) =>
      if (i % 50 == 0) println(i)
      doTheSearch(initialState, myValves, 26).pressure + doTheSearch(initialState, elephantValves, 26).pressure
    }.max

    println(p2)
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
}
