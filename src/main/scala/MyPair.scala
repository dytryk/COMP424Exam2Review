import scala.collection.AbstractIterator

@main def pairTest(): Unit = {
  val p = new Pair(3, 5)
  println(s"p.size -> ${p.size}")
  println(s"p.head -> ${p.head}")
  println(s"p.tail -> [${p.tail.mkString(", ")}]")
  println(s"elements of p greater than 4: ${ p.filter(_ > 4).mkString(" ")}")
  println(s"${(p ++ (new Pair(7, 9))).mkString(" ")}")

  for e <- (p ++ new Pair(7, 9)) do println(e)
}

class Pair[A](val first: A, val second: A) extends Iterable[A] {
  def iterator: Iterator[A] = new AbstractIterator[A]:
    private var current = 0
    override def hasNext: Boolean = current < 2
    override def next(): A = {
      current += 1
      if current == 1 then first
      else if current == 2 then second
      else throw new RuntimeException("You can't to do that ...")
    }
}