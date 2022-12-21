package twentytwo.twentyone

import lib.Support

import scala.annotation.tailrec

object TwentytwoTwentyone extends App with Support {
  val testData =
    """
      |root: pppw + sjmn
      |dbpl: 5
      |cczh: sllz + lgvd
      |zczc: 2
      |ptdq: humn - dvpt
      |dvpt: 3
      |lfqf: 4
      |humn: 5
      |ljgn: 2
      |sjmn: drzm * dbpl
      |sllz: 4
      |pppw: cczh / lfqf
      |lgvd: ljgn * ptdq
      |drzm: hmdt - zczc
      |hmdt: 32
      |""".stripMargin.trim
  val input = load

  val numberR = """(\w+): (\d+)""".r
  val combR = """(\w+): (\w+) ([-+*/=]) (\w+)""".r

  def combine(op: String, a: Long, b: Long): Long = {
    op match {
      case "+"           => a + b
      case "-"           => a - b
      case "*"           => a * b
      case "/" if b != 0 => a / b
      case _             => throw new IllegalArgumentException(s"unknown operator $op")
    }
  }

  def createInverseOperation(op: String, a: TwentytwoTwentyone.MonkeyDoes, b: TwentytwoTwentyone.MonkeyDoes) =
    (op, a, b) match {
      case ("+", Number(x), OperationList(ops)) => ((n: Long) => n - x) +: ops
      case ("+", OperationList(ops), Number(x)) => ((n: Long) => n - x) +: ops
      case ("-", Number(x), OperationList(ops)) => ((n: Long) => x - n) +: ops
      case ("-", OperationList(ops), Number(x)) => ((n: Long) => n + x) +: ops
      case ("*", Number(x), OperationList(ops)) => ((n: Long) => n / x) +: ops
      case ("*", OperationList(ops), Number(x)) => ((n: Long) => n / x) +: ops
      case ("/", Number(x), OperationList(ops)) => ((n: Long) => x / n) +: ops
      case ("/", OperationList(ops), Number(x)) => ((n: Long) => x * n) +: ops
    }

  @tailrec def find(ms: List[String], heard: Map[String, Long]): Long = {
    heard.get("root") match {
      case Some(number) => number
      case None =>
        ms.head match {
          case numberR(name, value) => find(ms.tail, heard + (name -> value.toLong))
          case combR(name, depA, op, depB) =>
            (heard.get(depA), heard.get(depB)) match {
              case (Some(a), Some(b)) => find(ms.tail, heard + (name -> combine(op, a, b)))
              case _                  => find(ms.tail :+ ms.head, heard)
            }
        }
    }
  }

  def solveP2(pair: (MonkeyDoes, MonkeyDoes)): Long = {
    pair match {
      case (Number(n), OperationList(ops)) => ops.foldLeft(n)((m, op) => op(m))
      case (OperationList(ops), Number(n)) => ops.foldLeft(n)((m, op) => op(m))
    }
  }

  @tailrec def findP2(ms: List[String], heard: Map[String, MonkeyDoes], targets: (String, String)): Long = {
    (heard.get(targets._1), heard.get(targets._2)) match {
      case (Some(l), Some(r)) => solveP2((l, r))
      case _ =>
        ms.head match {
          case numberR("humn", _)   => findP2(ms.tail, heard + ("humn" -> OperationList(List())), targets)
          case numberR(name, value) => findP2(ms.tail, heard + (name -> Number(value.toLong)), targets)
          case combR(name, depA, op, depB) =>
            (heard.get(depA), heard.get(depB)) match {
              case (Some(Number(a)), Some(Number(b))) =>
                findP2(ms.tail, heard + (name -> Number(combine(op, a, b))), targets)
              case (Some(a), Some(b)) =>
                findP2(ms.tail, heard + (name -> OperationList(createInverseOperation(op, a, b))), targets)
              case _ => findP2(ms.tail :+ ms.head, heard, targets)
            }
        }
    }
  }

  sealed trait MonkeyDoes
  case class Number(n: Long) extends MonkeyDoes
  case class OperationList(fs: List[Long => Long]) extends MonkeyDoes

  def run(data: String) = {
    val in = stringSeq(data).toList

    val p1 = find(in, Map.empty)
    println(p1)

    val root = in.collectFirst { case combR("root", l, _, r) => (l, r) }.get
    val p2 = findP2(in.filterNot(_.contains("root")), Map.empty, root)
    println(p2)
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
}
