package twentytwo.two

import lib.Support

object TwentytwoTwo extends App with Support {

  def playRoundP1(r: (Char, Char)): Int = {
    val shapeScore = r._2 - 'X' + 1
    val winScore = r match {
      case ('A', 'Y') | ('B', 'Z') | ('C', 'X') => 6
      case ('A', 'X') | ('B', 'Y') | ('C', 'Z') => 3
      case _                                    => 0
    }
    shapeScore + winScore
  }

  def playRoundP2(r: (Char, Char)): Int = {
    val winScore = (r._2 - 'X') * 3
    val shapeScore = r match {
      case ('A', 'Z') | ('B', 'Y') | ('C', 'X') => 2 // paper
      case ('A', 'Y') | ('B', 'X') | ('C', 'Z') => 1 // rock
      case _                                    => 3 // scissors
    }

    winScore + shapeScore
  }

  val i = loadStringSeq.map(round => round.head -> round.apply(2))
  val p1 = i.map(playRoundP1).sum

  println(p1)

  val p2 = i.map(playRoundP2).sum
  println(p2)
}
