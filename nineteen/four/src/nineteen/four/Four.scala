package nineteen.four

import lib.Support

object Four extends App with Support {
  val in = loadIntSeq("-")
  val a = in.head
  val b = in.last

  def part1(range: Seq[Int]): Int = {
    range.count { n =>
      val pairs = n.toString.sliding(2).toSeq
      pairs.forall(pair => pair.head <= pair.last) && pairs.exists(pair =>
        pair.head == pair.last
      )
    }
  }

  def part2(range: Seq[Int]): Int = {
    range.count { n =>
      val pairs = n.toString.sliding(2).toSeq
      val quads = n.toString.sliding(4).toSeq
      val start = n.toString.take(3)
      val end = n.toString.drop(3)
      pairs.forall(pair => pair.head <= pair.last) && (
        (start.head == start(1) && start(1) != start(2)) ||
          (end.head != end(1) && end(1) == end.last) ||
          quads.exists(quad =>
            quad.head != quad(1) && quad(1) == quad(2) && quad(2) != quad(3)
          )
      )
    }
  }

  println(part1(a to b))
  println(part2(a to b))
}
