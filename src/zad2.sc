sealed trait lBT[+A]
case object LEmpty extends lBT[Nothing]
case class LNode[+A](elem:A, left:()=>lBT[A], right:()=>lBT[A]) extends lBT[A]

def lfrom(n:Int):Stream[Int] = {
  n #:: lfrom(n + 1)
}

// Zad 2
val lfib = {
  def lfibIn(p: Int, n: Int): Stream[Int] = {
    (p + n) #:: lfibIn(n, (p + n))
  }

  Stream.cons(0, Stream.cons(1, lfibIn(0, 1)))
}


println(lfib.take(15).toList)
