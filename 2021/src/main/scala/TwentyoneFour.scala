package twentyone.four

import lib.Support

import scala.annotation.tailrec

object TwentyoneFour extends App with Support {
  val i = loadStringSeq

  val numbers = i.head.split(",").map(_.toInt)

  val boards = i.tail.filter(_ != "").grouped(5).map(_.flatMap(_.sliding(2, 3).map(_.trim.toInt))).toSeq

  def getWinningGroups(board: Seq[Int]): Seq[Seq[Int]] = {
    board.grouped(5).toSeq ++ board.grouped(5).toSeq.transpose
  }

  @tailrec def findBingoWinner(boards: Seq[Seq[Int]], picked: Seq[Int], remaining: Seq[Int]): Int = {
    val pickedSet = picked.toSet
    val winner = boards.map(getWinningGroups).find(line => line.exists(_.toSet.subsetOf(pickedSet)))
    winner match {
      case Some(board) => board.flatten.toSet.diff(pickedSet).sum * picked.last
      case None        => findBingoWinner(boards, picked :+ remaining.head, remaining.tail)
    }
  }

  @tailrec def findFinalWinner(boards: Seq[Seq[Int]], picked: Seq[Int], remaining: Seq[Int], scores: Seq[Int]): Int = {
    val pickedSet = picked.toSet
    val (winners, rest) = boards.partition(board => getWinningGroups(board).exists(_.toSet.subsetOf(pickedSet)))
    val scoresThisRound = scores ++ winners.map(_.toSet.diff(pickedSet).sum * picked.last)
    if (remaining.isEmpty) {
      scores.last
    } else {
      findFinalWinner(rest, picked :+ remaining.head, remaining.tail, scoresThisRound)
    }
  }

  println(findBingoWinner(boards, Nil, numbers.toSeq))
  println(findFinalWinner(boards, Nil, numbers.toSeq, Nil))
}
