import lib.Support

import java.lang.Math.{max, min}
import scala.annotation.tailrec
import scala.collection.mutable

object d23 extends App with Support {
  sealed trait Space {
    val repr: Char
  }
  object Space {
    def parse(c: Char): Space = c match {
      case '.' => Empty
      case 'A' => Amber
      case 'B' => Bronze
      case 'C' => Copper
      case 'D' => Desert
    }
    def parse(s: String): Space = s match {
      case "." => Empty
      case "A" => Amber
      case "B" => Bronze
      case "C" => Copper
      case "D" => Desert
    }
  }
  case object Empty extends Space {
    override val repr: Char = '.'
  }
  sealed trait Amphipod extends Space {
    val energyMult: Int
  }
  case object Amber extends Amphipod {
    override val repr: Char = 'A'
    override val energyMult: Int = 1
  }
  case object Bronze extends Amphipod {
    override val repr: Char = 'B'
    override val energyMult: Int = 10
  }
  case object Copper extends Amphipod {
    override val repr: Char = 'C'
    override val energyMult: Int = 100
  }
  case object Desert extends Amphipod {
    override val repr: Char = 'D'
    override val energyMult: Int = 1000
  }

  final case class State(
      hallway: Seq[Space],
      roomA: List[Space],
      roomB: List[Space],
      roomC: List[Space],
      roomD: List[Space],
      energyExpended: Int,
      roomDepth: Int = 2
  ) {
    import State.roomEntrances

    def repr: String = s"h: ${hallway.map(_.repr).mkString} a: ${roomA.map(_.repr).mkString} b: ${roomB
        .map(_.repr)
        .mkString} c: ${roomC.map(_.repr).mkString} d: ${roomD.map(_.repr).mkString}"
    def isComplete: Boolean =
      hallway.forall(_ == Empty) && roomA.forall(_ == Amber) && roomB.forall(_ == Bronze) && roomC.forall(
        _ == Copper
      ) && roomD.forall(_ == Desert)

    def countEnergy(a: Int, b: Int, extra: Int): Int = max(a, b) - min(a, b) + extra

    def moveIntoRoom(hwi: Seq[(Space, Int)]) = hwi
      .flatMap(_ match {
        case (p: Amphipod, i) => Some(p -> i)
        case _                => None
      })
      .filter { case (pod, position) =>
        pod match {
          case Amber =>
            (roomA.isEmpty || roomA.size <= roomDepth - 1 && roomA.forall(_ == Amber)) && hallway
              .slice(min(position + 1, 2), max(position, 3))
              .forall(_ == Empty)
          case Bronze =>
            (roomB.isEmpty || roomB.size <= roomDepth - 1 && roomB.forall(_ == Bronze)) && hallway
              .slice(min(position + 1, 4), max(position, 5))
              .forall(_ == Empty)
          case Copper =>
            (roomC.isEmpty || roomC.size <= roomDepth - 1 && roomC.forall(_ == Copper)) && hallway
              .slice(min(position + 1, 6), max(position, 7))
              .forall(_ == Empty)
          case Desert =>
            (roomD.isEmpty || roomD.size <= roomDepth - 1 && roomD.forall(_ == Desert)) && hallway
              .slice(min(position + 1, 8), max(position, 9))
              .forall(_ == Empty)
        }
      }
      .map { case (pod, position) =>
        pod match {
          case Amber =>
            copy(
              hallway = hallway.updated(position, Empty),
              roomA = pod :: roomA,
              energyExpended = energyExpended + countEnergy(
                position,
                2,
                roomDepth - roomA.size
              ) * 1 /*,  history = history :+ this.copy(history=Seq.empty)*/
            )
          case Bronze =>
            copy(
              hallway = hallway.updated(position, Empty),
              roomB = pod :: roomB,
              energyExpended = energyExpended + countEnergy(
                position,
                4,
                roomDepth - roomB.size
              ) * 10 /*,   history = history :+ this.copy(history=Seq.empty)*/
            )
          case Copper =>
            copy(
              hallway = hallway.updated(position, Empty),
              roomC = pod :: roomC,
              energyExpended = energyExpended + countEnergy(
                position,
                6,
                roomDepth - roomC.size
              ) * 100 /*,   history = history :+ this.copy(history=Seq.empty)*/
            )
          case Desert =>
            copy(
              hallway = hallway.updated(position, Empty),
              roomD = pod :: roomD,
              energyExpended = energyExpended + countEnergy(
                position,
                8,
                roomDepth - roomD.size
              ) * 1000 /*,   history = history :+ this.copy(history=Seq.empty)*/
            )
        }
      }

    def generateStates: Seq[State] = {
      // move pods from hallway to their room
      if (Seq(2, 4, 6, 8).map(hallway).exists(_ != Empty)) {
        moveIntoRoom(Seq(2, 4, 6, 8).map(i => hallway(i) -> i))
      } else {
        val moveOutOfA = roomA.headOption match {
          case None                                    => Nil
          case Some(Amber) if roomA.forall(_ == Amber) => Nil
          case Some(pod: Amphipod) =>
            val availablePositions =
              (hallway.zipWithIndex.slice(0, 2).reverse.takeWhile(_._1 == Empty).reverse ++ hallway.zipWithIndex
                .drop(3)
                .takeWhile(_._1 == Empty)).map(_._2).filterNot(roomEntrances.contains)
            val withEnergies = availablePositions.map(pos => pos -> countEnergy(pos, 2, roomDepth + 1 - roomA.size))

            //        val strightToRoom
            withEnergies.map { case pos -> energy =>
              copy(
                hallway = hallway.updated(pos, pod),
                roomA = roomA.tail,
                energyExpended =
                  energyExpended + energy * pod.energyMult /*,   history = history :+ this.copy(history=Seq.empty)*/
              )
            }
        }

        val moveOutOfB = roomB.headOption match {
          case None                                      => Nil
          case Some(Bronze) if roomB.forall(_ == Bronze) => Nil
          case Some(pod: Amphipod) =>
            val availablePositions =
              (hallway.zipWithIndex.slice(0, 4).reverse.takeWhile(_._1 == Empty).reverse ++ hallway.zipWithIndex
                .drop(5)
                .takeWhile(_._1 == Empty)).map(_._2).filterNot(roomEntrances.contains)
            if (energyExpended == 0) println(availablePositions)
            val withEnergies = availablePositions.map(pos => pos -> countEnergy(pos, 4, roomDepth + 1 - roomB.size))
            withEnergies.map { case pos -> energy =>
              copy(
                hallway = hallway.updated(pos, pod),
                roomB = roomB.tail,
                energyExpended =
                  energyExpended + energy * pod.energyMult /*,   history = history :+ this.copy(history=Seq.empty)*/
              )
            }
        }

        val moveOutOfC = roomC.headOption match {
          case None                                      => Nil
          case Some(Copper) if roomC.forall(_ == Copper) => Nil
          case Some(pod: Amphipod) =>
            val availablePositions =
              (hallway.zipWithIndex.slice(0, 6).reverse.takeWhile(_._1 == Empty).reverse ++ hallway.zipWithIndex
                .drop(7)
                .takeWhile(_._1 == Empty)).map(_._2).filterNot(roomEntrances.contains)
            val withEnergies = availablePositions.map(pos => pos -> countEnergy(pos, 6, roomDepth + 1 - roomC.size))
            withEnergies.map { case pos -> energy =>
              copy(
                hallway = hallway.updated(pos, pod),
                roomC = roomC.tail,
                energyExpended =
                  energyExpended + energy * pod.energyMult /*,   history = history :+ this.copy(history=Seq.empty)*/
              )
            }
        }

        val moveOutOfD = roomD.headOption match {
          case None                                      => Nil
          case Some(Desert) if roomD.forall(_ == Desert) => Nil
          case Some(pod: Amphipod) =>
            val availablePositions =
              (hallway.zipWithIndex.slice(0, 8).reverse.takeWhile(_._1 == Empty).reverse ++ hallway.zipWithIndex
                .drop(9)
                .takeWhile(_._1 == Empty)).map(_._2).filterNot(roomEntrances.contains)
            val withEnergies = availablePositions.map(pos => pos -> countEnergy(pos, 8, roomDepth + 1 - roomD.size))
            withEnergies.map { case pos -> energy =>
              copy(
                hallway = hallway.updated(pos, pod),
                roomD = roomD.tail,
                energyExpended =
                  energyExpended + energy * pod.energyMult /*,   history = history :+ this.copy(history=Seq.empty)*/
              )
            }
        }

        moveIntoRoom(hallway.zipWithIndex) ++
          // move top pod from A to free space in hallway
          moveOutOfA ++
          // move top pod from B to free space in hallway
          moveOutOfB ++
          // move top pod from C to free space in hallway
          moveOutOfC ++
          // move top pod from D to free space in hallway
          moveOutOfD
      }
    }
  }
  object State {
    val roomEntrances = Set(2, 4, 6, 8)

    implicit object StateOrdering extends Ordering[State] {
      override def compare(x: State, y: State): Int = {
        /*val compSituated = x.situated compare y.situated
        if (compSituated != 0) compSituated
        else*/
        -(x.energyExpended compare y.energyExpended)
      }
    }
    def parse(input: String): State = {
      input match {
        case s"""#############
#${hallway}#
###${topa}#${topb}#${topc}#${topd}###
  #${bota}#${botb}#${botc}#${botd}#
  #########""" =>
          State(
            hallway = hallway.map(_ => Empty),
            roomA = List(topa, bota).map(Space.parse),
            roomB = List(topb, botb).map(Space.parse),
            roomC = List(topc, botc).map(Space.parse),
            roomD = List(topd, botd).map(Space.parse),
            energyExpended = 0
          )
      }
    }
  }
  @tailrec
  def solve(states: mutable.PriorityQueue[State], i: Int = 0, seenStates: Set[State]): Int = {
//    if (i % 1 == 0) println(states.size)
    if (states.isEmpty) println(seenStates.size)
    val state = states.dequeue()
    if (state.isComplete) {
//      println(state)
      println(s"done on it $i")
      println("history is")
//      state.history.foreach(h => println(h.repr))
      state.energyExpended
    } else {
      val gennedStates = state.generateStates.filter(st => !seenStates.contains(st))
      states.enqueue(gennedStates: _*)
      if (states.isEmpty) println(state.repr)
      solve(states, i + 1, seenStates ++ gennedStates)
    }
  }
  val i = load

  val initialState = State.parse(i)

  val part1 = solve(mutable.PriorityQueue(initialState), seenStates = Set.empty)

  println(part1)

  val part2state = initialState.copy(
    roomA = initialState.roomA.head +: List(Desert, Desert) :+ initialState.roomA.last,
    roomB = initialState.roomB.head +: List(Copper, Bronze) :+ initialState.roomB.last,
    roomC = initialState.roomC.head +: List(Bronze, Amber) :+ initialState.roomC.last,
    roomD = initialState.roomD.head +: List(Amber, Copper) :+ initialState.roomD.last,
    roomDepth = 4
  )
  println(part2state.repr)
  val part2 = solve(mutable.PriorityQueue(part2state), seenStates = Set.empty)
  println(part2)
}
