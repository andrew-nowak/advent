import lib._

import scala.annotation.tailrec

object d22 extends App with Support {
  final case class Brick(private val l: Coord3d, private val r: Coord3d) {
    val Seq(min, max) = Seq(l, r).sortBy(_.manhattan)
//    println(min, max)
    lazy val cubes = (for {
      x <- min.x to max.x
      y <- min.y to max.y
      z <- min.z to max.z
    } yield Coord3d(x, y, z)).toSet

    def drop(n: Int): Brick = {
      val lower = Coord3d(0, 0, -n)
      Brick(l + lower, r + lower)
    }

    override def toString: String = "Brick(" + min + "~" + max + ")"
  }

  val testData =
    """1,0,1~1,2,1
      |0,0,2~2,0,2
      |0,2,3~2,2,3
      |0,0,4~0,2,4
      |2,0,5~2,2,5
      |0,1,6~2,1,6
      |1,1,8~1,1,9""".stripMargin.trim
  val input = load

  final case class State(bs: Seq[Brick], cubes: Set[Coord3d])
  val ns = LazyList.from(1)

  @tailrec def dislodge(
      upDag: Map[Brick, Seq[Brick]],
      downDag: Map[Brick, Seq[Brick]],
      bricks: Set[Brick],
      droppedBricks: Set[Brick]
  ): Set[Brick] = {
    val allDropped = droppedBricks ++ bricks
    val above = bricks.flatMap(brick => upDag.getOrElse(brick, Seq.empty))
    val dropping =
      above.filter(downDag.get(_).exists(_.forall(allDropped.contains)))
    val newlyDropping = dropping diff allDropped
    if (newlyDropping.isEmpty) allDropped
    else dislodge(upDag, downDag, newlyDropping, allDropped)
  }

  def run(data: String): Unit = {
    val start = System.nanoTime()

    val in = stringSeq(data)

    val bricks = in
      .map { l =>
        val Array(start, end) = l.split("~").map { cs =>
          val Array(x, y, z) = cs.split(",")
          Coord3d(x.toInt, y.toInt, z.toInt)
        }
        Brick(start, end)
      }
      .sortBy(_.min.z)

    val dropped = bricks.tail
      .foldLeft(State(bricks.take(1), bricks.head.cubes)) {
        case (State(bricks, cubes), thisBrick) =>
          val drop = ns
            .find(n => {
              val droppedCubes = thisBrick.drop(n).cubes

              droppedCubes
                .exists(_.z <= 0) || droppedCubes.intersect(cubes).nonEmpty
            })
            .get - 1
          val droppedBrick = thisBrick.drop(drop)
          State(bricks :+ droppedBrick, cubes ++ droppedBrick.cubes)
      }
      .bs

    val bricksByFloor = dropped.groupBy(_.min.z)
    val bricksByRoof = dropped.groupBy(_.max.z)

    val p1 = dropped.filter { brick =>
      val roof = brick.max.z
      val nbors = bricksByRoof(roof)
      val supports = bricksByFloor
        .getOrElse(roof + 1, Seq.empty)
        .filter(
          _.drop(1).cubes.intersect(brick.cubes).nonEmpty
        )
      supports.forall { supported =>
        val howManyNeighboursSupport = nbors.count { nbor =>
          supported.drop(1).cubes.intersect(nbor.cubes).nonEmpty
        }
        howManyNeighboursSupport > 1
      }
    }

    println(p1.size)

    val p2Cands = dropped.diff(p1)

    val upDag: Map[Brick, Seq[Brick]] = dropped.map { brick =>
      brick -> dropped
        .filterNot(_ == brick)
        .filter(_.cubes.intersect(brick.drop(-1).cubes).nonEmpty)
    }.toMap
    val downDag: Map[Brick, Seq[Brick]] = dropped.map { brick =>
      brick -> dropped
        .filterNot(_ == brick)
        .filter(_.cubes.intersect(brick.drop(1).cubes).nonEmpty)
    }.toMap

    println(upDag.find { case (a, b) => b contains a })

    val p2 = p2Cands
      .map(cand => dislodge(upDag, downDag, Set(cand), Set.empty))
      .map(_.size - 1)
      .sum

    println(p2)

    val end = System.nanoTime()
    println(s"Done in ${(end - start).toDouble / 1_000_000} ms")
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
}
