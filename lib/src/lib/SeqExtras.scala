package lib

object SeqExtras {

  def produce[T, U](s: Seq[U])(z: T)(f: (T, U) => T) = {
    s.foldLeft(Seq(z))((acc, el) => acc :+ f(acc.last, el)).tail
  }

  def rotate[T](s: Seq[T], n: Int): Seq[T] = {
    val (bef, aft) = s.splitAt(n)
    aft ++ bef
  }
}
