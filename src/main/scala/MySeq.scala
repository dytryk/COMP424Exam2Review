import scala.collection
import scala.collection.mutable

trait MySeq[A] extends MyIterable[A] {
  def length: Int = size
  def apply(index: Int): A

  // TODO: define this method using only the iterator from MyIterable
  /**
   * Returns the index of the first occurrance of the item in the sequence
   * @param item the item to be located
   * @return the index where that item is located (or -1 if not found)
   */
  def indexOf(item: A): Int = {
    val it = iterator
    var index = 0
    while (it.hasNext) {
      if (it.next() == item) return index
      index += 1
    }
    -1
  }

  // TODO: define this method using only the iterator from MyIterable
  /**
   * Returns the index of the last occurrance of the item in the sequence
   *
   * @param item the item to be located
   * @return the index where that item is located (or -1 if not found)
   */
  def lastIndexOf(item: A): Int = {
    val it = iterator
    var lastIndex = -1
    var currentIndex = 0
    while (it.hasNext) {
      if (it.next() == item) lastIndex = currentIndex
      currentIndex += 1
    }
    lastIndex
  }

  // TODO: define this method using only the iterator from MyIterable
  /**
   * Returns the index of the first element for which the predicate is true
   *
   * @param pred a function that takes an item from the sequence and returns true or false
   * @return
   */
  def indexWhere(pred: A => Boolean): Int = {
    val it = iterator
    var index = 0
    while (it.hasNext) {
      if (pred(it.next())) return index
      index += 1
    }
    -1
  }

  // TODO: define this method using only the iterator from MyIterable

  /**
   * Returns the index of the last element for which the predicate is true
   *
   * @param pred a function that takes an item from the sequence and returns true or false
   * @return
   */
  def lastIndexWhere(pred: A => Boolean): Int = {
    val it = iterator
    var lastIndex = -1
    var currentIndex = 0
    while (it.hasNext) {
      if (pred(it.next())) lastIndex = currentIndex
      currentIndex += 1
    }
    lastIndex
  }

  // TODO: implement this using the iterator
  // NOTE: it is safe to use other known collections like mutable.Stack if they would be helpful...
  def reverse: Seq[A] = {
    val it = iterator
    var reversedList = List[A]()
    while (it.hasNext) reversedList = it.next() :: reversedList
    reversedList
  }

  // TODO: implement this using the iterator
  // NOTE: it is safe to use other known collections like mutable.Set if they would be helpful...
  def distinct: Seq[A] = {
    val it = iterator
    var distinctList = List[A]()
    val seenSet = mutable.HashSet[A]()
    while (it.hasNext) {
      val elem = it.next()
      if (!seenSet.contains(elem)) {
        seenSet.add(elem)
        distinctList = elem :: distinctList
      }
    }
    distinctList.reverse
  }

  // TODO: implement this using the iterator
  // NOTE: it is safe to use other known collections like mutable.ArrayBuffers if they would be helpful...
  /**
   * Returns a sorted copy of the collection sorted based on the provided comparison function
   *
   * @param compare comparison function that returns according to the java.lang.Comparable interface
   * @return an immutable sorted copy of the original sequence
   */
  def sorted(compare: (A, A) => Int): Seq[A] = {
    val it = iterator
    it.foldLeft(List.empty[A]) { (acc, elem) =>
      val (lower, higher) = acc.span(compare(_, elem) < 0)
      lower ::: (elem :: higher)
    }
  }
}
