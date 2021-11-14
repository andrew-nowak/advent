package lib

import scala.io.Source

trait Support {
  def loadDirty: String =
    Source.fromResource("input.txt").mkString

  def load: String = loadDirty.trim

  def loadStringSeq(delimiter: String): Seq[String] =
    load.split(delimiter).filter(_ != "")

  def loadStringSeq: Seq[String] = loadStringSeq("\\r?\\n")

  def loadIntSeq(delimiter: String = "\\r?\\n"): Seq[Int] =
    load
      .split(delimiter)
      .filter(_ != "")
      .map(_.toInt)

  def loadIntSeq: Seq[Int] = loadIntSeq()

  def load2dIntSeq(
      delimiterA: String = "\\r?\\n",
      delimiterB: String = " "
  ): Seq[Seq[Int]] =
    load
      .split(delimiterA)
      .filter(_ != "")
      .map(_.split(delimiterB).map(_.toInt).toSeq)

  def load2dIntSeq: Seq[Seq[Int]] = load2dIntSeq()
}
