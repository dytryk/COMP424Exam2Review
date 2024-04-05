import scala.collection.mutable

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

object LectureExcercises {
  //Homework 1
  def transcribeOld(s: String): String = for (c <- s) yield if (c == 'T') 'U' else c

  def transcribe(s: String): String = s.map((c: Char) => if c == 'T' then 'U' else c)

  def reverseComplementOld(s: String): String = for (c <- s.reverse) yield if (c == 'T') 'A' else if (c == 'A') 'T' else if (c == 'G') 'C' else 'G'

  def reverseComplement(s: String): String = s.reverse.map((c: Char) => if c == 'T' then 'A' else if c == 'A' then 'T' else if c == 'G' then 'C' else 'G')

  //Homework 2
  def countNucleotidesOld(s: String): Map[Char, Int] = {
    val countNucs = mutable.Map('A' -> 0, 'C' -> 0, 'G' -> 0, 'T' -> 0)
    for (c <- s) yield countNucs(c) += 1
    countNucs.toMap
  }

  def countNucleotides(s: String): Map[Char, Int] = {
    val countNucs = mutable.Map('A' -> 0, 'C' -> 0, 'G' -> 0, 'T' -> 0)
    s.map(countNucs(_) += 1)
    countNucs.toMap
  }

  def findMotifOld(s1: String, s2: String): List[Int] = {
    if (s1.equals("") || s2.equals("") || s2.length > s1.length) then return List()
    (for i <- 0 until 1 + s1.length - s2.length yield if s1.substring(i, i + s2.length) == s2 then i + 1 else -1).filter(_ >= 0).toList
  }

  def findMotif(s1: String, s2: String): List[Int] = {
    if (s1.equals("") || s2.equals("") || s2.length > s1.length) then return List()
    s1.sliding(s2.length).zipWithIndex.map { case (substr, i) => if (substr == s2) i + 1 else -1 }.filter(_ >= 0).toList
  }

  // Homework 3
  def profileOne(seq: Seq[String]): Seq[Map[Char, Int]] = {
    val inverseSeq: Seq[String] = seq.transpose.map(_.mkString)
    for (string <- inverseSeq) yield Map('A' -> string.count(_ == 'A'), 'C' -> string.count(_ == 'C'), 'G' -> string.count(_ == 'G'), 'T' -> string.count(_ == 'T')).filter { case (_, count) => count != 0 }
  }

  def profile(seq: Seq[String]): Seq[Map[Char, Int]] = {
    val inverseSeq: Seq[String] = seq.transpose.map(_.mkString)
    inverseSeq.map { string =>
      val counts = Map('A' -> 0, 'C' -> 0, 'G' -> 0, 'T' -> 0)
      string.foldLeft(counts) { (acc, char) => if (acc.contains(char)) acc.updated(char, acc(char) + 1) else acc }.filter { case (_, count) => count != 0 }
    }
  }

  def consensusOld(seq: Seq[String]): String = (for (map <- profile(seq)) yield map.maxBy(_._2)._1.toString).mkString

  def consensus(seq: Seq[String]): String = profile(seq).map((m: Map[Char, Int]) => m.maxBy(_._2)._1.toString).mkString

  //Homework 4
  def translateRnaOld(rna: String): String = {
    (for (i <- 0 until rna.length) yield if (i == 0 || i % 3 == 0) CodonTable.getAminoAcid(rna.substring(i, i + 3)) else "").filter(_ != "").filter(_ != "Stop").mkString
  }

  def translateRna(rna: String): String = {
    rna.zipWithIndex.map { case (_, i) => if (i == 0 || i % 3 == 0) CodonTable.getAminoAcid(rna.substring(i, i + 3)) else "" }.filter(_ != "").filter(_ != "Stop").mkString
  }
}