package lib

object Produce {

  def produce[T, U](s: Seq[U])(z: T)(f: (T, U) => T) = {
    s.foldLeft(Seq(z))((acc, el) => acc :+ f(acc.last, el)).tail
  }

  implicit class ProducingSeq[T](s: Seq[T]) {
    def produce(z: T)(f: (T, T) => T) = {
      s.foldLeft(Seq(z))((acc, el) => acc :+ f(acc.last, el)).tail
    }
  }
}
