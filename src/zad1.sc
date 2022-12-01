sealed trait lBT[+A]
case object LEmpty extends lBT[Nothing]
case class LNode[+A](elem:A, left:()=>lBT[A], right:()=>lBT[A]) extends lBT[A]

def lfrom(n:Int):Stream[Int] = {
  n #:: lfrom(n + 1)
}

// Zad 1
def lrepeat[T](n: Int, s: Stream[T]): Stream[T] = {
  def lrep(n: Int, x: T, streamRest: () => Stream[T]): Stream[T] = {
    if (n > 1) {
      x #:: lrep(n - 1, x, streamRest)
    } else {
      x #:: streamRest()
    }
  }

  if (s.isEmpty) {
    Stream.Empty
  } else {
    val x #:: xs = s
    lrep(n, x, () => lrepeat(n, xs))
  }
}

println(lfrom(0).take(6).toList)

println(lrepeat(3, lfrom(3)).take(10).toList)