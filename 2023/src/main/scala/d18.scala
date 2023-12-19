import lib.Direction._
import lib._

import scala.collection.immutable.Seq

object d18 extends App with Support {
  val testData =
    """
      |R 6 (#70c710)
      |D 5 (#0dc571)
      |L 2 (#5713f0)
      |D 2 (#d2c081)
      |R 2 (#59c680)
      |D 2 (#411b91)
      |L 5 (#8ceee2)
      |U 2 (#caa173)
      |L 1 (#1b58a2)
      |U 2 (#caa171)
      |R 2 (#7807d2)
      |U 3 (#a77fa3)
      |L 2 (#015232)
      |U 2 (#7a21e3)""".stripMargin.trim
  val input = load

  sealed trait VertexPairs {
    def span: (Int, Int)
    def matches(x: Int, y: Int): Boolean
  }
  case class Opening(a: Int, b: Int) extends VertexPairs {
    override def span = (a, b)

    override def matches(x: Int, y: Int): Boolean = false
  }
  case class Changing(a1: Int, a2: Int, b1: Int, b2: Int) extends VertexPairs {
    private val all = Seq(a1, a2, b1, b2)
    override def span = (all.min, all.max)

    override def matches(x: Int, y: Int): Boolean =
      (a1 == x && b1 == y) || (b1 == x && a1 == y)
  }
  case class Closing(a: Int, b: Int) extends VertexPairs {
    override def span = (a, b)

    override def matches(x: Int, y: Int): Boolean = a == x && b == y
  }
  case class Keeping(a: Int, b: Int) extends VertexPairs {
    override def span = (a, b)

    override def matches(x: Int, y: Int): Boolean = a == x && b == y
  }

  // TODO maybe openVertices can be SortedSet?
  final case class State(tot: Long, depth: Int, openVertices: Seq[Int])

  def calc(insts: Seq[(Direction, Int)]): Long = insts
    // build up list of vertices
    .scanLeft[Coord](Origin) { case (last, (d, n)) => last.go(d, n) }
    // don't include origin twice (and tail is faster than dropRight(1))
    .tail
    // group vertices into map of y-coord ("depth") -> list[x-coords]
    .groupMap(_.y)(_.x)
    // but don't actually want a map because want to iterate in order
    .toList
    .sortBy(_._1)
    //      .map(x => { println(x); x })
    // at each depth, group the x-coords into pairs
    .map { case (i, vertices) =>
      (
        i,
        vertices.sorted
          .grouped(2)
          .map { case Seq(a, b) => (a, b) }
          .toSeq
      )
    }
    // the main part. at each depth, figure out which columns have been "open"
    // since the last depth and multiply. then, figure out what tiles are
    // inside the shape at _this specific_ depth (very fiddly!).
    // then figure out which columns are open below this depth
    .foldLeft(State(0L, 0, Seq.empty)) {
      case (State(tot, lastDepth, openVertices), (depth, newVertices)) =>
        // add up the space since the last new vertices and here
        val spaceSince = openVertices
          .grouped(2)
          .map { case Seq(a, b) => b - a + 1 }
          .sum
          .toLong * math.max(0, depth - lastDepth - 1)

        //        println("depth" + depth)
        // for each pair of vertices currently open, figure out what happens
        // to the column at this depth. Might be closing (bottom of shape),
        // changing (widening, narrowing) or keeping (no change).
        // there's also opening (top of shape) but we can't spot that here.
        val pairs: Seq[VertexPairs] = openVertices
          .grouped(2)
          .flatMap { case Seq(a1, b1) =>
            // if the x-coords match in the currently open column and the
            // changing vertices at this depth, then we're "closing" the shape
            if (newVertices.contains((a1, b1))) Some(Closing(a1, b1))
            else {
              val ma2 = newVertices.collectFirst {
                case (na, nb) if na == a1 => nb
                case (na, nb) if nb == a1 => na
              }
              val mb2 = newVertices.collectFirst {
                case (na, nb) if na == b1 => nb
                case (na, nb) if nb == b1 => na
              }
              // widening narrowing or not changing at all
              (ma2, mb2) match {
                case (Some(a2), Some(b2)) => Some(Changing(a1, a2, b1, b2))
                case (Some(a2), _)        => Some(Changing(a1, a2, b1, b1))
                case (_, Some(b2))        => Some(Changing(a1, a1, b1, b2))
                case _                    => Some(Keeping(a1, b1))
              }
            }
          }
          .toSeq ++ newVertices.collect {
          // if there's vertices at this depth but no open columns above, then
          // this is an "opening" -- top of a section of shape
          case (a, b)
              if !openVertices.contains(a) && !openVertices.contains(b) =>
            Opening(a, b)
        }
        /* the tricky bit - figure out how many tiles are inside the shape at
           THIS depth
          x--y a--b
          x-a---y-b
          x-a---b-y
         */
        val spanHere = pairs
          .map(_.span)
          .sortBy(_._1)
          .foldLeft(List.empty[(Int, Int)]) {
            case (Nil, (a, b))                      => (a, b) :: Nil
            case ((x, y) :: tail, (a, b)) if a <= y => (x, y.max(b)) :: tail
            case (l, e)                             => e :: l
          }
          .map(p => p._2 - p._1 + 1)
          .sum

        // the other tricky part - what if two open columns merge!!
        /*
           ###   ###
           # #   # #
           # #   # #
           # ##### # <- here
           #       #
           #########
         */
        // eliminations because you're "eliminating" a pair of "openVertices"
        val eliminations = pairs
          .sliding(2)
          .toSeq
          .flatMap {
            case Seq(Changing(a1, a2, b1, b2), Changing(oa1, oa2, ob1, ob2))
                if (b1, b2) == (oa2, oa1) =>
              Seq(b1, b2)
            case Seq(Changing(a1, a2, b1, b2), Changing(oa1, oa2, ob1, ob2))
                if (ob1, ob2) == (a2, a1) =>
              Seq(ob1, ob2)
            case o => Seq.empty
          }
          .toSet
        val newOpenVertices =
          (openVertices.grouped(2).flatMap { case Seq(a, b) =>
            pairs.find(_.matches(a, b)) match {
              case Some(Changing(a1, a2, b1, b2)) => Some(Seq(a2, b2))
              case Some(Closing(a, b))            => None
              case Some(Opening(a, b))            => ???
              case Some(Keeping(a, b))            => Some(Seq(a, b))
              //            case None => Some(Seq(a, b))
            }
          } ++ pairs.collect { case Opening(a, b) => Seq(a, b) }).flatten.toSet
            .diff(eliminations)
            .toSeq
            .sorted

//      println("d" + depth, newOpenVertices.size)
//      println(depth, spaceSince, spanHere, pairs, openVertices, newOpenVertices)
        //        println(tot, spaceSince, spanHere)

        State(
          tot = tot + spaceSince + spanHere,
          depth = depth,
          openVertices = newOpenVertices
        )
    }
    .tot

  def run(data: String): Unit = {
    val start = System.nanoTime()

    val in = stringSeq(data)

    val p1r = """([LRUD]) (\d+) .*""".r

    val insts: Seq[(Direction, Int)] = in.map {
      case p1r("L", n) => (Left, n.toInt)
      case p1r("R", n) => (Right, n.toInt)
      case p1r("U", n) => (Up, n.toInt)
      case p1r("D", n) => (Down, n.toInt)
    }

//    val trench = insts.foldLeft(Seq(Coord(0, 0))) { case (acc@last :: _, (d, n)) =>
//      SeqExtras.produce(1 to n)(last)((prev, _) => prev.go(d)).reverse ++ acc
//    }.toSet

//    printCoords(trench)
//    println(startLoc)

    val p1 = calc(insts)
    println(p1)
//    println(p1 + trench.size)

    val p2r = """.* \(#([0-9a-f]{5})([0-3])\)""".r

    val p2Insts = in.map {
      case p2r(dist, "0") => (Right, Integer.parseInt(dist, 16))
      case p2r(dist, "1") => (Down, Integer.parseInt(dist, 16))
      case p2r(dist, "2") => (Left, Integer.parseInt(dist, 16))
      case p2r(dist, "3") => (Up, Integer.parseInt(dist, 16))
    }

    val p2 = calc(p2Insts)
    println(p2)

    val end = System.nanoTime()
    println(s"Done in ${(end - start).toDouble / 1_000_000} ms")
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)

}
