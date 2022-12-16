package twentytwo.sixteen

import lib.Support

import scala.annotation.tailrec

case class Valve(name: String, flow: Int, tunnels: Seq[String])
//noinspection DuplicatedCode
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

  case class State(location: String, openValves: Seq[String], time: Int, pressure: Int, recentlyVisited: Seq[String])
  case class P2State(locationA: String, locationB: String, openValves: Set[Valve], time: Int, pressure: Int, recentlyVisited: Set[String])
  def run(data: String) = {
    val in = stringSeq(data)
    val valveLookup = in.map { case parseR(name, flow, tunnels) => name -> Valve(name, flow.toInt, tunnels.split(", ").toSeq) }.toMap
    val valves = valveLookup.values.toSet
    val flowableValves = valves.filter(_.flow > 0).map(_.name)
    val flows = valves.map(_.flow).filterNot(_ == 0)
    val maxFlow = valves.map(_.flow).sum
    val flowableValvesSize = flowableValves.size
    val start = "AA"
    val initialState = State(start, Seq.empty, 0, 0, Seq.empty)
    var n = 0

    @tailrec def search(q: List[State], best: Int): Int = {
      if (q.isEmpty) best
      else {
        val state = q.head
        val pressure = state.pressure + state.openValves.map(valveLookup).map(_.flow).sum
        if (state.time == 30) {
          search(q.tail, Math.max(best, state.pressure))
        }
        else {
          val recentlyVisited = state.recentlyVisited :+ state.location
          val openHere = if (!(state.openValves contains state.location) && flowableValves.contains(state.location)) Some(state.copy(openValves = state.openValves :+ state.location, recentlyVisited = Seq.empty)) else None
          val moves = valveLookup(state.location).tunnels.filterNot(recentlyVisited contains _).map(tun => state.copy(location = tun, recentlyVisited = recentlyVisited))
          val nextStates = (openHere ++ moves).map(_.copy(time = state.time + 1, pressure = pressure)).toList
          search(nextStates ++ q.tail, best)
        }
      }
    }

//    val p1 = search(List(initialState), 0)
//    println(p1)

    def potential(state: P2State): Int = {
      state.pressure + // released pressure
        state.openValves.map(_.flow).sum * (26 - state.time) + // this turn
//        maxFlow * (26 - 1 - state.time)
        valves.diff(state.openValves).toSeq.map(_.flow).filterNot(_ == 0).sorted.reverse.zipWithIndex.map { case (f, i) => (26 - (state.time + 1 + (i / 2))) * f}.sum
    }

    @tailrec def p2Search(q: List[P2State], best: Int): Int = {
      n += 1

      if (q.isEmpty) best
      else {
        val state = q.head
        if (state.time == 26) {
          if (state.pressure > best) println(state.pressure)
          p2Search(q.tail, Math.max(best, state.pressure))
        } else if (state.openValves.size == flowableValvesSize) {
          // all valves open; do no moves and fastforward
          val pressure = state.pressure + state.openValves.map(_.flow).sum * (26 - state.time)
          p2Search(q.tail, Math.max(best, pressure))
//                  } else if (state.pressure + state.openValves.map(_.flow).sum + maxFlow * (26 - 1 - state.time) <= best) { // impossible to beat current best
        } else if (best > 0 && potential(state) <= best) {
          p2Search(q.tail, best)
        } else {
          val pressure = state.pressure + state.openValves.map(_.flow).sum
          val recentlyVisited = state.recentlyVisited + state.locationA + state.locationB
          val openHereA = if (!(state.openValves.map(_.name) contains state.locationA) && flowableValves.contains(state.locationA)) Some(
            state.copy(openValves = state.openValves + valveLookup(state.locationA))
          ) else None
          val openHereB = if (!(state.openValves.map(_.name) contains state.locationB) && flowableValves.contains(state.locationB)) Some(
            state.copy(openValves = state.openValves + valveLookup(state.locationB))
          ) else None
          val movesA = valveLookup(state.locationA).tunnels.filterNot(recentlyVisited.contains).map(
            tun => state.copy(locationA = tun)
          )
          val movesB = valveLookup(state.locationB).tunnels.filterNot(recentlyVisited.contains).map(
            tun => state.copy(locationB = tun)
          )
          val statesA = (openHereA ++ movesA)
          val statesB = (openHereB ++ movesB)
          val nextStates = (for { a <- statesA; b <- statesB if a.locationA != b.locationB } yield {
            val openValves = a.openValves | b.openValves
            a.copy(
              locationB = b.locationB,
              openValves = openValves,
              recentlyVisited = if (openValves.size > state.openValves.size) Set.empty else recentlyVisited,
              time = state.time + 1,
              pressure = pressure
            )
          }).toList
//          val nextStates = ().map(_.copy(time = state.time + 1, pressure = pressure)).toList
          p2Search(nextStates ++ q.tail, best)
        }
      }
    }
    val initialP2 = P2State(start, start, Set.empty, 0, 0, Set.empty)

    val p2 = p2Search(List(initialP2), 0)
    println(p2)
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
}
