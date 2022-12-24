package twentyone.nineteen

import lib.Support

import scala.jdk.CollectionConverters._

final case class Coord3d(x: Int, y: Int, z: Int)

object TwentyoneNineteen extends App with Support:
  val i = load

  val scanners = i
    .split("\\n?---.*---\\n")
    .filterNot(_.isEmpty)
    .map(rawRead =>
      rawRead
        .split(endl)
        .map(_.trim)
        .filterNot(_.isEmpty)
        .map(beaconRead =>
          beaconRead.split(",").map(_.toInt) match
            case Array(x, y, z) => Coord3d(x, y, z)
            case _              => throw new RuntimeException(s"invalid scanner result $beaconRead")
        )
        .toSeq
    )
    .zipWithIndex

  scanners.foreach { case (scanner, i) =>
    println(s"Scanner $i")
    scanner.foreach(println)
    println("")
    println("")
  }
