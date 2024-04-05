@main
def tests(): Unit = {
  println("should be true: " + parity(List(true, true, false, true, false, false)))
  println("should be false: " + parity(List(true, true, false, true, false, false, true)))
  println("max should be 110: " + max(List(1, 3, 5, 34, 110, -34, -2244, 3, 0)))
  println("should be 1.2, 2.2, 3.2, 4.2: " + transform(List(1, 2, 3, 4), (x => x+0.2)))
  println("should be true: " + both((x: Int) => x % 2 == 0, (x: Int) => x > 0)(2))
  println("should be false: " + both((x: Int) => x % 2 == 0, (x: Int) => x > 0)(23))
  println("should be 3: " + makeCounter(5)(List(5, 4, 6, 3, 7, 2, 8)))
  println("should be 25.0: " + localMax((x: Int) => (10*x - x*x), 0, 10))
  println("should be true: " + all(List((x: Int) => x % 2 == 0, (x: Int) => x > 0))(4))
  println("should be false: " + all(List((x: Int) => x % 2 == 0, (x: Int) => x > 0))(1))
  println("should be 5: " + argMax((x: Int) => 10*x - x*x, 0, 10))
  println("should be W3nd/: " + makeString(Seq('W', '3', 'n', 'd', '/')))
  println("should be 12345: " + makeString(Seq(1, 2, 3, 4, 5)))
  println("should be W3nd/: " + makeStringReduce(Seq('W', '3', 'n', 'd', '/')))
  println("should be 12345: " + makeStringReduce(Seq(1, 2, 3, 4, 5)))
  println("should be W 3 n d /: " + makeStringDelim(Seq('W', '3', 'n', 'd', '/'), " "))
  println("should be 1_2_3_4_5: " + makeStringDelim(Seq(1, 2, 3, 4, 5), "_"))
}

def parity(bools: List[Boolean]): Boolean = {
  bools.foldLeft(false)(_^_)
}

def max(nums: List[Int]): Int = {
  nums.foldLeft(Int.MinValue)((y, x) => if x > y then x else y)
}

def transform(xs: List[Int], f: Int => Double): List[Double] = {
  for x <- xs yield f(x)
}

def both[A](f1: A => Boolean, f2: A => Boolean): A => Boolean = {
  (x: A) => f1(x) && f2(x)
}

def all[A](predicates: Iterable[A => Boolean]): A => Boolean = {
  predicates.foldLeft((x: A) => true)(both)
}

def makeCounter(x: Int): List[Int] => Int = {
  (xs: List[Int]) => xs.count(_ > x)
}

def localMax(f: Int => Double, min: Int, max: Int): Double = {
  (min until max).map(f).max
}

def argMax(f: Int => Double, min: Int, max: Int): Int = {
  (min until max).maxBy(f)
}

def makeString[A](xs: Seq[A]): String = {
  xs.foldLeft("")(_+_)
}

def makeStringReduce[A](xs: Seq[A]): String = {
  xs.map(_.toString).reduce(_+_)
}

def makeStringDelim[A](xs: Seq[A], delim: String = ""): String = {
  xs.foldLeft("")((x, y) => x + (if x != "" then delim + y.toString else y.toString))
}

/**
 * REWRITTEN HOMEWORKS USING .filter() AND .map() INSTEAD OF FOR-COMPREHENSIONS
 */

