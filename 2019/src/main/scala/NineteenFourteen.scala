package nineteen.fourteen

import lib.Support
import scala.annotation.tailrec

case class Chemical(name: String, quantity: Long)

object NineteenFourteen extends App with Support {
  val testData =
    """
      |171 ORE => 8 CNZTR
      |7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
      |114 ORE => 4 BHXH
      |14 VRPVC => 6 BMBT
      |6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
      |6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
      |15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
      |13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
      |5 BMBT => 4 WPTQ
      |189 ORE => 9 KTJDG
      |1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
      |12 VRPVC, 27 CNZTR => 2 XDBXC
      |15 KTJDG, 12 BHXH => 5 XCVML
      |3 BHXH, 2 VRPVC => 7 MZWV
      |121 ORE => 7 VRPVC
      |7 XCVML => 6 RJRHP
      |5 BHXH, 4 VRPVC => 5 LTCX
      |""".stripMargin.trim
  val input = load

  type Recipes = Seq[(Chemical, Seq[Chemical])]

  @tailrec def search(target: Long, min: Long, max: Long, recipes: Recipes): Long = {
    if (min == max) min - 1
    else if (min + 1 == max) min
    else {
      val n = (max + min) / 2L
      println(n)
      val res = requirements(Seq(Chemical("FUEL", n)), recipes)
      if (res < target) search(target, n, max, recipes)
      else if (res > target) search(target, min, n, recipes)
      else n
    }
  }

  def requirements(needs: Seq[Chemical], recipes: Recipes): Long = {

    def requirements(needs: Seq[Chemical], ore: Long, extras: Seq[Chemical]): Long = {
      if (needs.isEmpty) ore
      else {
        val need = needs.head
        val production = recipes.find(_._1.name == need.name).get
        val spare = extras.find(_.name == need.name).map(_.quantity).getOrElse(0L)
        val needQ = need.quantity - spare
        val times = (needQ.toDouble / production._1.quantity.toDouble).ceil.toInt
        val leftoverQ = production._1.quantity * times - needQ - spare
        val (newNeeds, oreNeeds) =
          production._2.map(ingr => Chemical(ingr.name, ingr.quantity * times)).partition(nd => nd.name != "ORE")
        val newExtras = (extras :+ Chemical(need.name, leftoverQ))
          .groupMapReduce(_.name)(_.quantity)(_ + _)
          .map { case (name, q) => Chemical(name, q) }
          .toSeq

        requirements(needs.tail ++ newNeeds, ore + oreNeeds.map(_.quantity).sum, newExtras)
      }
    }

    requirements(needs, 0, Seq.empty)
  }

  def run(data: String) = {
    val in = stringSeq(data)

    val recipes = in.map { line =>
      val Array(bef, aft) = line.split(" => ")

      val product = {
        val Array(n, name) = aft.split(" ")
        Chemical(name, n.toInt)
      }
      val ingredients = bef
        .split(", ")
        .map { part =>
          val Array(n, name) = part.split(" ")
          Chemical(name, n.toInt)
        }
        .toSeq
      product -> ingredients
    }

    val p1 = requirements(Seq(Chemical("FUEL", 1)), recipes)
    println(p1)

    println(search(1_000_000_000_000L, 0, 10_000_000L, recipes))

  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
}
