

// Zad 2
val lfib = {
  def lfibIn(p: Int, n: Int): LazyList[Int] = {
    (p + n) #:: lfibIn(n, (p + n))
  }

  LazyList.cons(0, LazyList.cons(1, lfibIn(0, 1)))
}


println(lfib.take(15).toList)
