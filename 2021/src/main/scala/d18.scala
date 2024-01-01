import lib.Support
import play.api.libs.json._
import play.api.libs.functional.syntax._

import scala.annotation.tailrec

sealed trait ExplosionResponse {}
case object NoExplosion extends ExplosionResponse
final case class ExplosionFinished(content: SnailfishNumber) extends ExplosionResponse
final case class Exploding(content: SnailfishNumber, pair: (Option[Int], Option[Int])) extends ExplosionResponse
object Exploding {
  def apply(content: SnailfishNumber, pair: (Option[Int], Option[Int])): ExplosionResponse = {
    pair match {
      case (None, None) => ExplosionFinished(content)
      case _            => new Exploding(content, pair)
    }
  }
}

sealed trait SplitResponse {}
case object NoSplit extends SplitResponse
case class DoneSplit(content: SnailfishNumber) extends SplitResponse

sealed trait SnailfishNumber {
  def magnitude: Int
  def addLeft(amount: Int): SnailfishNumber = {
    this match {
      case Number(value) => Number(value + amount)
      case Pair(l, r)    => Pair(l.addLeft(amount), r)
    }
  }
  def addRight(amount: Int): SnailfishNumber = {
    this match {
      case Number(value) => Number(value + amount)
      case Pair(l, r)    => Pair(l, r.addRight(amount))
    }
  }

  def +(other: SnailfishNumber): SnailfishNumber =
    Pair(this, other).reduce

  @tailrec
  final def reduce: SnailfishNumber = {
    this match {
      case n: Number => n
      case p: Pair =>
        val toReduce = p.explode.orElse(p.split)
        toReduce match {
          case Some(needsReducing) => needsReducing.reduce
          case None                => p
        }
    }
  }

  def doExplode(depth: Int): ExplosionResponse
  def split: Option[SnailfishNumber]
}
object SnailfishNumber {
  implicit val reads: Reads[SnailfishNumber] =
    __.read[Int].map(Number) or
      __.lazyRead(Reads.seq[SnailfishNumber](reads)).map(l => Pair(l.head, l(1)))
}
final case class Pair(left: SnailfishNumber, right: SnailfishNumber) extends SnailfishNumber {
  override def magnitude: Int = (left.magnitude * 3) + (right.magnitude * 2)
  def explode: Option[SnailfishNumber] = doExplode(0) match {
    case NoExplosion                => None
    case ExplosionFinished(content) => Some(content)
    case Exploding(content, _)      => Some(content)
  }

  def doExplode(depth: Int): ExplosionResponse = {
    require(depth <= 4)
    if (depth == 4) {
      require(left.isInstanceOf[Number] && right.isInstanceOf[Number], s"depth was $depth, left $left, right $right")
      Exploding(Number(0), (Some(left.asInstanceOf[Number].value), Some(right.asInstanceOf[Number].value)))
    } else {
      left.doExplode(depth + 1) match {
        case Exploding(content, (l, Some(re))) => Exploding(Pair(content, right.addLeft(re)), (l, None))
        case Exploding(content, (l, None))     => Exploding(Pair(content, right), (l, None))
        case ExplosionFinished(content)        => ExplosionFinished(Pair(content, right))
        case NoExplosion =>
          right.doExplode(depth + 1) match {
            case Exploding(content, (Some(le), r)) => Exploding(Pair(left.addRight(le), content), (None, r))
            case Exploding(content, (None, r))     => Exploding(Pair(left, content), (None, r))
            case ExplosionFinished(content)        => ExplosionFinished(Pair(left, content))
            case NoExplosion                       => NoExplosion
          }
      }
    }
  }

  override def split: Option[SnailfishNumber] =
    left.split.map(spv => Pair(spv, right)) orElse right.split.map(spv => Pair(left, spv))

}

final case class Number(value: Int) extends SnailfishNumber {
  override def doExplode(depth: Int): ExplosionResponse = NoExplosion

  override def magnitude: Int = value

  override def split: Option[SnailfishNumber] =
    if (value >= 10)
      Some(Pair(Number(value / 2), Number((value / 2) + (value % 2))))
    else
      None
}

object d18 extends App with Support {
  import SnailfishNumber.reads
  val i = loadStringSeq

  def snailfishSum(ts: Seq[SnailfishNumber]) = ts.reduce(_ + _).magnitude

  val terms = i.map(line => Json.parse(line).as[SnailfishNumber])

  val part1 = snailfishSum(terms)
  println(part1)

  val part2 = terms.combinations(2).flatMap(pair => Seq(pair, pair.reverse)).map(snailfishSum).max
  println(part2)
}
