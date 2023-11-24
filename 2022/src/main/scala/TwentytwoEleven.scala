package twentytwo.eleven

import lib.Support

import scala.collection.mutable

case class Monkey(
    items: mutable.ArrayBuffer[Long],
    op: Long => Long,
    test: Long => Int,
    var inspections: Long = 0
)
object Monkey {
  def apply(items: Seq[Long], op: Long => Long, div: Long, a: Int, b: Int): Monkey =
    Monkey(mutable.ArrayBuffer.from(items), op, n => if (n % div == 0L) a else b)
}

object Monkeys {
  def round(monkeys: Seq[Monkey], rounds: Int, relievable: Boolean): Seq[Monkey] = {
    val rem: Long = 2 * 13 * 3 * 17 * 19 * 7 * 11 * 5 * 23
    for (_ <- 0 until rounds) {
      for (monkey <- monkeys) {
        for (item <- monkey.items) {
          monkey.inspections += 1
          val newWorry = if (relievable) monkey.op(item) / 3L else monkey.op(item)
          val p2Worry = if (!relievable) newWorry % rem else newWorry
          val dest = monkey.test(newWorry)
          monkeys(dest).items.addOne(p2Worry)
        }
        monkey.items.clear()
      }
    }
    monkeys
  }
}

object TwentytwoEleven extends App with Support {
  val testData = Seq(
    Monkey(Seq(79, 98), _ * 19L, 23, 2, 3),
    Monkey(Seq(54, 65, 75, 74), _ + 6L, 19, 2, 0),
    Monkey(Seq(79, 60, 97), n => n * n, 13, 1, 3),
    Monkey(Seq(74), _ + 3L, 17, 0, 1)
  )
  val realData = Seq(
    // snip
  )

  def run(data: Seq[Monkey]) = {

    val p1data = data.map(monkey =>
      monkey.copy(
        items = mutable.ArrayBuffer.from(monkey.items),
        inspections = 0
      )
    )
    val p1 = Monkeys.round(p1data, 20, true)
    println(p1.sortBy(_.inspections).takeRight(2).map(_.inspections))

    val p2data = data.map(monkey =>
      monkey.copy(
        items = mutable.ArrayBuffer.from(monkey.items),
        inspections = 0
      )
    )
    val p2 = Monkeys.round(p2data, 10000, false)
    println(p2.map(_.inspections).sorted.takeRight(2).product)
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(realData)
}
