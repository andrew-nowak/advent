package lib

object SeqExtras {

  def produce[T, U](s: Seq[U])(z: T)(f: (T, U) => T) = {
    s.foldLeft(Seq(z))((acc, el) => acc :+ f(acc.last, el)).tail
  }

  def thread[T, U, V](s: Seq[T])(z: U)(f: (T, U) => (V, U)): (Seq[V], U) = {
    s.foldLeft(Seq.empty[V], z)((acc, el) => {
      val (v, u) = f(el, acc._2)
      (acc._1 :+ v, u)
    })
  }

  def rotate[T](s: Seq[T], n: Int): Seq[T] = {
    val (bef, aft) = s.splitAt(n)
    aft ++ bef
  }

  implicit class SeqExtensions[T](s: Seq[T]) {
    def produce[U](z: U)(f: (U, T) => U): Seq[U] = SeqExtras.produce(s)(z)(f)
    def thread[U, V](z: U)(f: (T, U) => (V, U)): (Seq[V], U) = SeqExtras.thread(s)(z)(f)
    def frequencies: Map[T, Int] = s.groupBy(identity).map { case (k, v) => (k, v.size) }
  }
}
