package nineteen.one

import lib.Support

object One extends App with Support {

  def calculateFuel(mass: Int): Int = (mass / 3) - 2

  val input = loadIntSeq()

  println(input.map(calculateFuel).sum)

  def calculateFuelFuel(mass: Int): Int =
    LazyList.iterate(calculateFuel(mass))(calculateFuel).takeWhile(_ > 0).sum

  println(input.map(calculateFuelFuel).sum)
}
