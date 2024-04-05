import scala.annotation.targetName
import scala.collection
import scala.collection.mutable

trait MyIterable[A] {
  def iterator: Iterator[A]

  // provide a concrete implementation of size based solely on iterator
  def size: Int = {
    var i = 0
    val it = iterator
    while (it.hasNext) {
      it.next()
      i += 1
    }
    i
  }

  // provide a concrete implementation of isEmpty based solely on iterator
  def isEmpty: Boolean = !iterator.hasNext

  // TODO: provide a concrete implementation of head based on iterator and isEmpty (throws NoSuchElementException if empty)
  def head: A = {
    if isEmpty then throw new NoSuchElementException("")
    iterator.next()
  }

  // TODO: provide a concrete implementation of tail based solely on iterator (throws UnsupportedOperationException on no head)
  // HINT: there is a mutable version of list called mutable.ListBuffer (turn it into an immutable list before returning)
  def tail: Iterable[A] = {
    if isEmpty then throw new UnsupportedOperationException("")
    val it = iterator
    it.next()
    val out = mutable.ListBuffer[A]()
    while it.hasNext do out.addOne(it.next())
    out.toList
  }

  // TODO: provide a concrete implementation of filter based solely on iterator
  def filter(f: A => Boolean): Iterable[A] = {
    val out = mutable.ListBuffer[A]()
    val it = iterator
    while (it.hasNext) {
      val e = it.next()
      if (f(e)) {
        out.addOne(e)
      }
    }
    out.toList
  }

  // bad version
//  def filter(f: A => Boolean): Iterable[A] = {
//    val out = mutable.ListBuffer[A]()
//    val it = iterator
//    while (it.hasNext) {
//      if (f(it.next())) {
//        out.addOne(it.next())
//      }
//    }
//    out.toList
//  }

  // TODO: provide a concrete implementation of mkString based solely on iterator
  def mkString(delim: String = ""): String = {
    if isEmpty then return ""
    val it = iterator
    var out: String = it.next().toString
    while it.hasNext do out = out + delim + it.next().toString
    out
  }

  // provide a concrete implementation of concat based solely on iterators
  @targetName("concat")
  def ++(other: MyIterable[A]): Iterable[A] = {
    val myIT = iterator
    val out = mutable.ListBuffer[A]()
    while myIT.hasNext do out.addOne(myIT.next())
    val theirIT = other.iterator
    while theirIT.hasNext do out.addOne(theirIT.next())
    out.toList
  }

  // TODO: provide a concrete implementation of contains based solely on iterator
  def contains(e: A): Boolean = {
    val it = iterator
    while it.hasNext do if  it.next() == e then return true
    false
  }

  // TODO: provide a concrete implementation of map based solely on iterator
  def map[B](f: A => B): Iterable[B] = {
    val out = mutable.ListBuffer[B]()
    val it = iterator
    while it.hasNext do out.addOne(f(it.next()))
    out.toList
  }
}