def lfrom(n:Int):LazyList[Int] = {
  n #:: lfrom(n + 1)
}

// Zad 1
def lrepeat[T](n: Int, s: LazyList[T]): LazyList[T] = {
  def lrep(i: Int, x: T, remaining: () => LazyList[T]): LazyList[T] = {
    if (i > 1) {
      x #:: lrep(i - 1, x, remaining)
    } else {
      x #:: remaining()
    }
  }

  if (s.isEmpty) {
    LazyList.empty
  } else {
    val x #:: xs = s
    lrep(n, x, () => lrepeat(n, xs))
  }
}

println(lfrom(0).take(6).toList)

println(lrepeat(3, lfrom(3)).take(10).toList)