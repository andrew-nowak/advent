package nineteen.seventeen

import lib.Intcode
import lib.Support

object NineteenSeventeen extends App with Support {
  val input = load

  def run(data: String) = {
    val in = longSeq(data, ",")

    val p1 = Intcode.buildAndRunWithIo(in, Nil)

    val map = p1.map(_.toChar)

    val width = map.indexOf('\n')

    val intersections = map.zipWithIndex.flatMap {
      case ('#', i) =>
        val lookup = map.lift
        val neighbours = (
          lookup(i - 1),
          lookup(i + 1),
          lookup(i - width - 1),
          lookup(i + width + 1)
        )
        neighbours match {
          case (Some('#'), Some('#'), Some('#'), Some('#')) =>
            Some((i % (width + 1)) * (i / (width + 1)))
          case _ =>
            None
        }
      case _ => None
    }

    println("   " + (0 to 4).flatMap(List.fill(10)(_)).mkString + '5')
    println("   " + List.fill(5)("0123456789").flatten.mkString + '0')
    println(
      map.mkString
        .split('\n')
        .zipWithIndex
        .map {
          case (row, i) if i <= 9 => s" $i $row  $i"
          case (row, i)           => s"$i $row $i"
        }
        .mkString("\n")
    )
    println("   " + (0 to 4).flatMap(List.fill(10)(_)).mkString + '5')
    println("   " + List.fill(5)("0123456789").flatten.mkString + '0')
    println(intersections.sum)

    val p2mem = in.updated(0, 2L)
    // worked out by hand
    val p2input = List(
      "A,B,A,B,C,C,B,A,B,C",
      "L,12,L,6,L,8,R,6", // A
      "L,8,L,8,R,4,R,6,R,6", // B
      "L,12,R,6,L,8", // C
      "n" // no videofeed pls
    ).mkString("", "\n", "\n").map(_.toLong).toList
    val p2 = Intcode.buildAndRunWithIo(p2mem, p2input)
    println(p2.last)
  }

  println("--- real ---")
  run(input)
}
