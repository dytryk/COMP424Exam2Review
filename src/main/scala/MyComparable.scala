@main def comparableTesting(): Unit = {
  val p1 = new R2Point(1.0, 1.0) with MyComparable[R2Point]
  val p2 = new R2Point(2.0, 2.0) with MyComparable[R2Point]

  println(s"${p1} < ${p2} | ${p1 mylt p2}")
  println(s"${p1} > ${p2} | ${p1 mygt p2}")
  println(s"${p1} == ${p2} | ${p1 myeq p2}")
  println(s"${p1} <= ${p2} | ${p1 mylte p2}")
  println(s"${p1} >= ${p2} | ${p1 mygte p2}")
}

trait MyComparable[T] {
  // abstract method compare
  def mycompare(o: T): Int

  /* TODO define concrete methods for mylt, mygt, myeq, mylte, and mygte */
  def mylt(o: T): Boolean = mycompare(o) < 0
  def mygt(o: T): Boolean = mycompare(o) > 0
  def myeq(o: T): Boolean = mycompare(o) == 0
  def mylte(o: T): Boolean = mycompare(o) <= 0
  def mygte(o: T): Boolean = mycompare(o) >= 0
}

class R2Point(val x: Double, val y: Double) {
  /* TODO define concrete implementation of compare
          where p1 > p2 iff sqrt(p1.x^2+p1.y^2) > sqrt(p2.x^2+p2.y^2)
  */
  def mycompare(o: R2Point): Int = {
    val m1 = math.sqrt(math.pow(this.x, 2) + math.pow(this.y, 2))
    val m2 = math.sqrt(math.pow(o.x, 2) + math.pow(o.y, 2))
    // TODO: return either -1, 0, or 1 following Java standards
    // https://docs.oracle.com/javase/8/docs/api/java/util/Comparator.html#compare-T-T-
    if m1 > m2 then 1 else if m2 > m1 then -1 else 0
  }

  override def toString: String = s"($x, $y)"
}