package twentyone.two

import lib.Support

final case class Position(y: Int, x: Int) {
  def product: Int = y * x

  def forward(dist: Int): Position = copy(x = x + dist)
  def up(dist: Int): Position = copy(y = y - dist)
  def down(dist: Int): Position = copy(y = y + dist)
}

final case class AimedPosition(y: Int, x: Int, aim: Int) {
  def product: Int = y * x

  def forward(dist: Int): AimedPosition = copy(
    y = y + dist * aim,
    x = x + dist
  )
  def up(by: Int): AimedPosition = copy(aim = aim - by)
  def down(by: Int): AimedPosition = copy(aim = aim + by)
}

object TwentyoneTwo extends App with Support {
  val i = loadStringSeq
  val instructionPattern = "(forward|down|up) (-?\\d+)".r

  val part1 = i
    .foldLeft(Position(0, 0)) { (pos, instruction) =>
      instruction match {
        case instructionPattern("forward", n) => pos.forward(n.toInt)
        case instructionPattern("down", n)    => pos.down(n.toInt)
        case instructionPattern("up", n)      => pos.up(n.toInt)
      }
    }
    .product

  println(part1)

  val part2 = i
    .foldLeft(AimedPosition(0, 0, 0)) { (pos, instruction) =>
      instruction match {
        case instructionPattern("forward", n) => pos.forward(n.toInt)
        case instructionPattern("up", n)      => pos.up(n.toInt)
        case instructionPattern("down", n)    => pos.down(n.toInt)
      }
    }
    .product

  println(part2)
}
