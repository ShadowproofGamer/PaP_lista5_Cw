sealed trait lBT[+A]
case object LEmpty extends lBT[Nothing]
case class LNode[+A](elem:A, left:()=>lBT[A], right:()=>lBT[A]) extends lBT[A]

def lfrom(n:Int):Stream[Int] = {
  n #:: lfrom(n + 1)
}

//Zad 3

def lTree(n: Int): lBT[Int] = {
  LNode(n, () => lTree(2 * n), () => lTree(2 * n + 1))
}
def bfs[T](ltree: lBT[T]): Stream[T] = {
  def bfsIn(queue: List[lBT[T]]): Stream[T] = {
    queue match {
      case Nil => Stream.Empty
      case LEmpty :: t => bfsIn(t)
      case LNode(v, l, r) :: t => Stream.cons(v, bfsIn(t ++ List(l(), r())))
    }
  }

  bfsIn(List(ltree))
}

println(bfs(lTree(5)).take(7).toList)